;;; pi-deck-workers.el --- Orchestration panels and workers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pi's Emacs orchestration host starts workers (Pilish sessions), forks
;; (Pilish sessions placed as "interactive-fork") and panels (terminals) as
;; buffers no window shows.  pi-deck links them to their parent agent and
;; places them in the parent's space as they start: panels below the
;; parent's pane when that is on screen, else in a new background tab;
;; forks in a new background tab.  Workers get no tab: they are listed
;; under their parent and shown beside it on demand.  Once a worker, fork or
;; panel closes, the windows and tabs showing only it go too.
;;
;; Workers are ephemeral: the host kills their buffers when they finish.
;; Their session files remain, so each closed worker is remembered under its
;; parent and can be read in Pilish's read-only saved-conversation view.

;;; Code:

(require 'json)
(require 'pi-deck-core)
(require 'pi-deck-spaces)

(declare-function pi-deck--client-frames "pi-deck-daemon" ())
(declare-function pi-deck--remove-views "pi-deck-close" (buffers))
(declare-function pilish-open-subagent-snapshot "pilish-snapshot" (&optional target))
(declare-function pilish--snapshot-target-for-pi-v3 "pilish-core"
                  (anchor-file session-id root file))

(defcustom pi-deck-show-panels t
  "Whether terminals (panels) started by an agent are shown at once.
A panel opens below its parent agent's pane when that pane is on screen,
otherwise in a new tab of the parent's space, without taking focus."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-panel-height 12
  "Height in lines of a panel window opened below its parent's pane."
  :type 'natnum
  :group 'pi-deck)

(defcustom pi-deck-finished-workers-limit 100
  "Maximum number of closed workers remembered across all parents."
  :type 'natnum
  :group 'pi-deck)

(defvar pi-deck-sidebar-buffer-name)

(defvar pi-deck--finished-workers nil
  "Closed orchestration Pi workers, newest first.
Plists with :name, :parent (the parent agent's buffer), :session-file and
:time.")

;;;; Panels

(defun pi-deck--target (id)
  "Return orchestration target ID as listed by `pi-orchestration-targets'."
  (when (fboundp 'pi-orchestration-targets)
    (cl-find id (pi-orchestration-targets)
             :key (lambda (target) (plist-get target :id)) :test #'equal)))

(defun pi-deck--display-frame ()
  "Return the client frame new panels appear in, or nil."
  (let ((frames (pi-deck--client-frames)))
    (if (memq (selected-frame) frames) (selected-frame) (car frames))))

(defun pi-deck--panel-space (buffer parent)
  "Return the space of panel BUFFER whose parent agent is PARENT (or nil)."
  (let ((agent (pi-deck-agent buffer)))
    (or (and agent (pi-deck-agent-space agent))
        (and parent (pi-deck-agent-space parent)))))

(defun pi-deck--split-below (window)
  "Split a panel window off the bottom of WINDOW, or return nil.
The panel gets `pi-deck-panel-height' lines, or a third of WINDOW."
  (let ((height (min pi-deck-panel-height
                     (/ (window-total-height window) 3))))
    (and (>= height window-min-height)
         (ignore-errors (split-window window (- height) 'below)))))

(defun pi-deck--panel-window-below (buffer parent)
  "Show BUFFER below PARENT's pane in the selected tab; return the window.
When that pane is too small, split the tab's main window instead.  Return
nil when PARENT is not on screen or there is no room."
  (when-let* ((parent-window (and parent (pi-deck--input-or-chat-window parent)))
              (window (or (pi-deck--split-below (pi-deck--pane-of parent-window))
                          (pi-deck--split-below (window-main-window)))))
    (set-window-buffer window buffer)
    window))

(defun pi-deck--background-tab (agent space)
  "Open a tab of SPACE showing AGENT, then return to the selected tab."
  (let ((original (tab-bar--current-tab-index)))
    (pi-deck--new-tab-in-space (or space "other"))
    (pi-deck--display-agent-in-window agent (selected-window))
    (let ((new (tab-bar--current-tab-index)))
      (tab-bar-select-tab (1+ (if (<= new original) (1+ original) original))))))

(defun pi-deck-show-panel (buffer &optional parent-buffer frame)
  "Show panel BUFFER of PARENT-BUFFER's agent in FRAME without selecting it.
FRAME defaults to the selected client frame.  Nothing happens when BUFFER is
already shown there or no client frame exists."
  (let ((frame (or frame (pi-deck--display-frame))))
    (when (and frame (buffer-live-p buffer) (not (get-buffer-window buffer frame)))
      (with-selected-frame frame
        (save-selected-window
          (let ((parent (pi-deck-agent parent-buffer)))
            (unless (pi-deck--panel-window-below buffer parent)
              (pi-deck--background-tab (pi-deck--track buffer 'shell)
                                       (pi-deck--panel-space buffer parent)))))
        (pi-deck--schedule-update)))))

(defun pi-deck-show-fork (chat &optional parent-buffer frame)
  "Show the forked Pi session CHAT in a new tab of PARENT-BUFFER's space.
The tab opens in FRAME (default the selected client frame) without being
selected."
  (let ((frame (or frame (pi-deck--display-frame)))
        (agent (pi-deck--track chat 'pi)))
    (when (and frame (buffer-live-p chat) (not (get-buffer-window chat frame)))
      (with-selected-frame frame
        (save-selected-window
          (let ((parent (pi-deck-agent parent-buffer)))
            (setf (pi-deck-agent-assigned-space agent) (and parent (pi-deck-agent-space parent)))
            (pi-deck--background-tab agent (pi-deck-agent-assigned-space agent))))
        (pi-deck--schedule-update)))))

(defun pi-deck--on-target (id event)
  "Refresh agents after orchestration target ID's EVENT; place new targets.
Panels open beside their parent and forks in a tab of its space; workers
are only listed."
  (pi-deck-refresh)
  (when (eq event 'start)
    (when-let* ((target (pi-deck--target id))
                (buffer (plist-get target :buffer)))
      ;; Leave emacsclient's evaluation before rearranging windows.
      (cond
       ((and (equal (plist-get target :kind) "shell") pi-deck-show-panels)
        (run-at-time 0 nil #'pi-deck-show-panel buffer (plist-get target :parent)))
       ((and (equal (plist-get target :kind) "pi")
             (equal (plist-get target :placement) "interactive-fork"))
        (run-at-time 0 nil #'pi-deck-show-fork buffer (plist-get target :parent)))))))

;;;; Closing

(defun pi-deck--remember-worker (agent)
  "Remember closed worker AGENT under its parent when it has a session file."
  (let ((file (pi-deck-agent-session-file agent))
        (parent (pi-deck-agent-parent agent)))
    (when (and (stringp file) (file-readable-p file) (buffer-live-p parent))
      (push (list :name (pi-deck-agent-name agent) :parent parent
                  :session-file file :time (float-time))
            pi-deck--finished-workers)
      (setq pi-deck--finished-workers
            (seq-take pi-deck--finished-workers pi-deck-finished-workers-limit)))))

(defun pi-deck--on-kill-buffer ()
  "Forget the current buffer's agent; remember it when it was a worker.
Views of closing orchestration targets are removed.  Records of the
buffer's own closed workers are dropped with it."
  (let ((buffer (current-buffer)))
    (when-let* ((agent (gethash buffer pi-deck--agents)))
      ;; Pi closes its workers, forks and panels itself: their views go.
      (when (pi-deck-agent-target agent)
        (with-demoted-errors "pi-deck: %S"
          (pi-deck--remove-views (pi-deck-agent-windows-buffers agent))))
      (when (and (pi-deck-agent-target agent) (eq (pi-deck-agent-kind agent) 'pi))
        (with-demoted-errors "pi-deck: %S" (pi-deck--remember-worker agent)))
      (remhash buffer pi-deck--agents)
      (pi-deck--schedule-update))
    (when (cl-find buffer pi-deck--finished-workers
                   :key (lambda (record) (plist-get record :parent)))
      (setq pi-deck--finished-workers
            (cl-remove buffer pi-deck--finished-workers
                       :key (lambda (record) (plist-get record :parent))))
      (pi-deck--schedule-update))))

(defun pi-deck-finished-workers (parent)
  "Return the closed workers remembered for PARENT's buffer, newest first."
  (cl-remove-if-not (lambda (record) (eq (plist-get record :parent) parent))
                    pi-deck--finished-workers))

(defun pi-deck--session-file-id (file)
  "Return the session ID in the header line of Pi session FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 65536)
    (goto-char (point-min))
    (plist-get (json-parse-string (buffer-substring (point) (line-end-position))
                                  :object-type 'plist)
               :id)))

(defun pi-deck-open-finished-worker (record)
  "Open closed worker RECORD's session in Pilish's read-only view."
  (let* ((file (plist-get record :session-file))
         (target (and (file-readable-p file)
                      (pilish--snapshot-target-for-pi-v3
                       file (pi-deck--session-file-id file)
                       (file-name-directory file) (file-name-nondirectory file)))))
    (unless target (user-error "Session of %s is unavailable" (plist-get record :name)))
    ;; Side windows cannot be split for the view.
    (pi-deck--select-ordinary-window)
    (pilish-open-subagent-snapshot target)))

(provide 'pi-deck-workers)
;;; pi-deck-workers.el ends here
