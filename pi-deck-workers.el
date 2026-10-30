;;; pi-deck-workers.el --- Orchestration panels and workers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Pi's Emacs orchestration host starts workers (Pilish sessions), forks
;; (Pilish sessions placed as "interactive-fork") and panels (terminals) as
;; buffers no window shows.  pi-deck links them to their parent agent and
;; its space: a panel opens in half of the parent's pane when that is on
;; screen; forks, workers and panels whose parent is hidden are listed in
;; the sidebar and shown on demand.  Once a worker, fork or panel closes,
;; the panes showing it go too.
;;
;; Workers are ephemeral: the host kills their buffers when they finish.
;; Their session files remain, so each closed worker is remembered under its
;; parent.  It can be read in Pilish's read-only saved-conversation view, or
;; resumed as an independent session whose results stay with you.

;;; Code:

(require 'json)
(require 'pi-deck-core)
(require 'pi-deck-spaces)

(declare-function pi-deck--client-frames "pi-deck-daemon" ())
(declare-function pi-deck--remove-views "pi-deck-close" (buffers))
(declare-function pilish-view-session-file "pilish" (file))
(declare-function pilish-start-session "pilish" (directory name &rest options))

(defcustom pi-deck-show-panels t
  "Whether terminals (panels) started by an agent are shown at once.
A panel takes half of its parent agent's pane, without taking focus, when
that pane is on screen; otherwise it is only listed in the sidebar."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-finished-workers-limit 100
  "Maximum number of closed workers remembered across all parents."
  :type 'natnum
  :group 'pi-deck)

(defvar pi-deck--finished-workers nil
  "Closed orchestration Pi workers, newest first.
Plists with :name, :parent (the parent agent's buffer), :session-file,
:directory and :time.")

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

(defun pi-deck--panel-direction (window)
  "Return where a panel splits off WINDOW's pane, as Herdr does.
`right' when the pane is at least twice as wide as it is high, else `below'."
  (let ((pane (pi-deck--pane-of window)))
    (if (>= (window-total-width pane) (* 2 (window-total-height pane))) 'right 'below)))

(defun pi-deck--panel-window-beside (buffer parent)
  "Show BUFFER in half of PARENT's pane in the selected frame; return the window.
Return nil when PARENT is not on screen or its pane is too small to split."
  (when-let* ((parent-window (and parent (pi-deck--input-or-chat-window parent)))
              (window (ignore-errors
                        (pi-deck--split-pane parent-window
                                             (pi-deck--panel-direction parent-window)))))
    (set-window-buffer window buffer)
    window))

(defun pi-deck-show-panel (buffer &optional parent-buffer frame)
  "Show panel BUFFER beside PARENT-BUFFER's agent in FRAME without selecting it.
FRAME defaults to the selected client frame.  Nothing happens when BUFFER is
already shown there, the parent is not on screen or no client frame exists."
  (let ((frame (or frame (pi-deck--display-frame))))
    (when (and frame (buffer-live-p buffer) (not (get-buffer-window buffer frame)))
      (with-selected-frame frame
        (save-selected-window
          (pi-deck--panel-window-beside buffer (pi-deck-agent parent-buffer)))
        (pi-deck--schedule-update)))))

(defun pi-deck--on-target (id event)
  "Refresh agents after orchestration target ID's EVENT; place new panels.
Panels open beside their parent when it is on screen.  Forks belong to
their parent's space; they and workers are only listed in the sidebar."
  (pi-deck-refresh)
  (when (eq event 'start)
    (when-let* ((target (pi-deck--target id))
                (buffer (plist-get target :buffer)))
      ;; Leave emacsclient's evaluation before rearranging windows.
      (cond
       ((and (equal (plist-get target :kind) "shell") pi-deck-show-panels)
        (run-at-time 0 nil #'pi-deck-show-panel buffer (plist-get target :parent)))
       ((equal (plist-get target :placement) "interactive-fork")
        (when-let* ((parent (pi-deck-agent (plist-get target :parent))))
          (setf (pi-deck-agent-assigned-space (pi-deck--track buffer 'pi))
                (pi-deck-agent-space parent))))))))

;;;; Closing

(defun pi-deck--remember-worker (agent)
  "Remember closed worker AGENT under its parent when it has a session file."
  (let ((file (pi-deck-agent-session-file agent))
        (parent (pi-deck-agent-parent agent)))
    (when (and (stringp file) (file-readable-p file) (buffer-live-p parent))
      (push (list :name (pi-deck-agent-name agent) :parent parent
                  :session-file file :directory (pi-deck-agent-directory agent)
                  :time (float-time))
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

(defun pi-deck-open-finished-worker (record)
  "Open closed worker RECORD's session in Pilish's read-only view."
  (let ((file (plist-get record :session-file)))
    (unless (file-readable-p file)
      (user-error "Session of %s is unavailable" (plist-get record :name)))
    ;; Side windows cannot be split for the view.
    (pi-deck--select-ordinary-window)
    (pilish-view-session-file file)))

(defun pi-deck--session-agent (file)
  "Return the live Pi agent that has session FILE open, or nil."
  (let ((file (file-truename file)))
    (cl-find-if (lambda (agent)
                  (when-let* ((open (pi-deck-agent-session-file agent)))
                    (equal (file-truename open) file)))
                (pi-deck-agents))))

(defun pi-deck-resume-finished-worker (record)
  "Reopen closed worker RECORD's session as an ordinary editable Pi session.
It starts in the worker's directory and its parent's space, in a new pane.
The session is independent: nothing it does returns to the parent.  When
the session is already open, jump to it instead.  Return its agent."
  (let ((file (plist-get record :session-file))
        (dir (plist-get record :directory)))
    (unless (and (stringp file) (file-readable-p file)
                 (stringp dir) (file-directory-p dir))
      (user-error "Session of %s is unavailable" (plist-get record :name)))
    (let ((open (pi-deck--session-agent file)))
      (if open
          (pi-deck-reveal open)
        (let* ((parent (pi-deck-agent (plist-get record :parent)))
               (space (or (and parent (pi-deck-agent-space parent))
                          (pi-deck-current-space)))
               (chat (pilish-start-session dir (pi-deck--unique-session-name dir)
                                           :session-file file))
               (agent (pi-deck--track chat 'pi)))
          (setf (pi-deck-agent-assigned-space agent) space)
          (when (and space (not (equal space (pi-deck-current-space))))
            (pi-deck-select-space space))
          (pi-deck--display-agent-in-window agent (pi-deck--new-pane-window))
          (select-window (pi-deck--input-or-chat-window agent))
          (setq open agent)))
      open)))

(provide 'pi-deck-workers)
;;; pi-deck-workers.el ends here
