;;; pi-deck-workers.el --- Orchestration panels and workers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pi's Emacs orchestration host starts workers (Pilish sessions) and
;; panels (terminals) as buffers no window shows.  pi-deck links them to
;; their parent agent, shows panels in the parent's space as soon as they
;; start (below the parent's pane when that is on screen, else in a new
;; background tab of the space), and removes the windows and tabs it made
;; for them once they close.
;;
;; Workers are ephemeral: the host kills their buffers when they finish.
;; Their session files remain, so each closed worker is remembered under its
;; parent and can be read in Pilish's read-only saved-conversation view.

;;; Code:

(require 'json)
(require 'pi-deck-core)
(require 'pi-deck-spaces)

(declare-function pi-deck--client-frames "pi-deck-daemon" ())
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

(defvar pi-deck--shown-panels nil
  "Panel buffers pi-deck opened windows or tabs for.")

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
    (set-window-parameter window 'pi-deck-panel (buffer-name buffer))
    (push buffer pi-deck--shown-panels)
    window))

(defun pi-deck--panel-background-tab (buffer space)
  "Open a tab of SPACE showing BUFFER, then return to the selected tab."
  (let ((original (tab-bar--current-tab-index)))
    (pi-deck--new-tab-in-space (or space "other") (buffer-name buffer))
    (set-window-buffer (selected-window) buffer)
    (setf (alist-get 'pi-deck-panel (cdr (tab-bar--current-tab-find))) (buffer-name buffer))
    (push buffer pi-deck--shown-panels)
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
              (pi-deck--panel-background-tab buffer (pi-deck--panel-space buffer parent)))))
        (pi-deck--schedule-update)))))

(defun pi-deck--on-target (id event)
  "Refresh agents after orchestration target ID's EVENT; show new panels."
  (pi-deck-refresh)
  (when (and (eq event 'start) pi-deck-show-panels)
    (when-let* ((target (pi-deck--target id))
                ((equal (plist-get target :kind) "shell"))
                (buffer (plist-get target :buffer)))
      ;; Leave emacsclient's evaluation before rearranging windows.
      (run-at-time 0 nil #'pi-deck-show-panel buffer (plist-get target :parent)))))

;;;; Closing

(defun pi-deck--remove-panel-views (buffer)
  "Delete the windows and tabs pi-deck opened for panel BUFFER.
Panel tabs close only while they show nothing but BUFFER (and the sidebar)."
  (let ((name (buffer-name buffer)))
    (dolist (frame (pi-deck--client-frames))
      (dolist (window (window-list frame 'no-minibuf))
        (when (and (equal (window-parameter window 'pi-deck-panel) name)
                   (eq (window-buffer window) buffer)
                   (not (frame-root-window-p window)))
          (delete-window window)))
      (with-selected-frame frame
        (let ((index 0) close)
          (dolist (tab (pi-deck--tabs))
            (cl-incf index)
            (when (and (equal (alist-get 'pi-deck-panel tab) name)
                       (cl-every (lambda (shown)
                                   (or (eq shown buffer)
                                       (equal (buffer-name shown) pi-deck-sidebar-buffer-name)))
                                 (pi-deck--tab-buffers tab frame)))
              (push index close)))
          ;; Highest index first, so earlier indices stay valid.
          (dolist (index close)
            (tab-bar-close-tab index)))))))

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
Records of the buffer's own closed workers are dropped with it."
  (let ((buffer (current-buffer)))
    (when (memq buffer pi-deck--shown-panels)
      (setq pi-deck--shown-panels (delq buffer pi-deck--shown-panels))
      (with-demoted-errors "pi-deck: %S" (pi-deck--remove-panel-views buffer)))
    (when-let* ((agent (gethash buffer pi-deck--agents)))
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
    (pilish-open-subagent-snapshot target)))

(provide 'pi-deck-workers)
;;; pi-deck-workers.el ends here
