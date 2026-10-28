;;; pi-deck-persist.el --- Save and restore spaces, tabs and sessions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The state file records registered spaces, the tabs of a client frame
;; (group, name and writable window state), Pilish sessions with their
;; session files, and terminals with their directory and command.
;; Restoring resumes Pi sessions from those files, restarts terminals, and
;; recreates missing spaces' tabs.  Orchestration workers and panels belong
;; to their parent Pi and are never restored.
;;
;; Tabs are frame-local, but a daemon may have no client frame.  The last
;; client frame's tabs are therefore kept when it closes and given to the
;; next new frame, and other new frames copy the layout of an existing one.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-overview)

(declare-function pi-deck--client-frames "pi-deck-daemon" ())

(defcustom pi-deck-state-file (locate-user-emacs-file "var/pi-deck/state.eld")
  "File holding saved spaces, tabs and sessions."
  :type 'file
  :group 'pi-deck)

(defcustom pi-deck-auto-restore nil
  "Whether enabling `pi-deck-mode' restores the saved state.
A daemon restarted by `pi-deck-restart-daemon' restores regardless."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-autosave-interval 60
  "Seconds between automatic state saves, or nil to save only on exit."
  :type '(choice (const nil) number)
  :group 'pi-deck)

(defcustom pi-deck-mirror-new-frames t
  "Whether new client frames receive the tabs of an existing or last frame."
  :type 'boolean
  :group 'pi-deck)

(defvar pi-deck--last-tabs nil
  "Last known tabs of a client frame, refreshed by every save and when a
client frame closes.  New frames and saves without any client frame use it.")

(defvar pi-deck--pending-tabs nil
  "Restored tabs waiting for a client frame.")

(defun pi-deck--restore-marker ()
  "Return the file marking a restore after daemon restart."
  (concat pi-deck-state-file ".restore"))

(defun pi-deck-mark-restore-pending ()
  "Make the next `pi-deck-mode' activation restore the saved state."
  (make-directory (file-name-directory pi-deck-state-file) t)
  (write-region "" nil (pi-deck--restore-marker) nil 'silent))

;;;; Serialization

(defun pi-deck--agent-record (agent tab-index)
  "Return the restorable record of AGENT, or nil.
TAB-INDEX caches `pi-deck-tab-index'."
  (let ((buffer (pi-deck-agent-buffer agent))
        (space (pi-deck-agent-space agent tab-index)))
    (unless (pi-deck-agent-target agent)
      (pcase (pi-deck-agent-kind agent)
        ('pi
         (list :kind 'pi :buffer (buffer-name buffer)
               :dir (pi-deck-agent-directory agent)
               :name (pi-deck-agent-session-name agent)
               :session-file (pi-deck-agent-session-file agent) :space space))
        ('shell
         (list :kind 'shell :buffer (buffer-name buffer)
               :dir (pi-deck-agent-directory agent)
               :command (or (buffer-local-value 'pi-deck--shell-command buffer)
                            (list (pi-deck-shell-program)))
               :space space))))))

(defun pi-deck-frame-tabs (&optional frame)
  "Return FRAME's root and space tabs as restorable plists.
Tabs outside spaces stay local to their frame: they are typically a new
frame's initial tab, and restoring or mirroring them only adds clutter."
  (let ((frame (or frame (selected-frame))))
    (cl-remove-if
     (lambda (record) (null (plist-get record :group)))
     (mapcar (lambda (tab)
               (let ((current (eq (car tab) 'current-tab)))
                 (list :name (alist-get 'name tab)
                       :explicit (alist-get 'explicit-name tab)
                       :group (pi-deck--tab-group tab)
                       :current current
                       :root (pi-deck-root-tab-p tab)
                       :ws (if current
                               (window-state-get (frame-root-window frame) t)
                             (alist-get 'ws tab)))))
             (pi-deck--tabs frame)))))

(defun pi-deck--state-frame ()
  "Return the client frame whose tabs represent the deck, or nil."
  (let ((frames (pi-deck--client-frames)))
    (if (memq (selected-frame) frames) (selected-frame) (car frames))))

(defun pi-deck-state ()
  "Return the current deck state."
  (let ((tab-index (pi-deck-tab-index (pi-deck--state-frame)))
        (frame (pi-deck--state-frame)))
    (list :version 1
          :spaces pi-deck-spaces
          :sidebar pi-deck--sidebar-visible
          ;; Without a client frame, keep the last known tabs: a terminal
          ;; that dies (closed SSH, killed multiplexer) deletes its frames
          ;; without `delete-frame-functions'.
          :tabs (if frame
                    (setq pi-deck--last-tabs (pi-deck-frame-tabs frame))
                  (or pi-deck--pending-tabs pi-deck--last-tabs))
          :agents (delq nil (mapcar (lambda (agent) (pi-deck--agent-record agent tab-index))
                                    (pi-deck-agents))))))

;;;###autoload
(defun pi-deck-save-state (&optional file)
  "Save spaces, tabs and sessions to FILE (default `pi-deck-state-file')."
  (interactive)
  (let ((file (or file pi-deck-state-file))
        (state (pi-deck-state)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (let ((print-length nil) (print-level nil) (print-circle nil))
        (insert ";; pi-deck state -*- lisp-data -*-\n")
        (prin1 state (current-buffer))
        (insert "\n")))
    (when (called-interactively-p 'any)
      (message "Saved %d spaces and %d sessions to %s"
               (length (plist-get state :spaces)) (length (plist-get state :agents))
               (abbreviate-file-name file)))
    state))

(defun pi-deck-read-state (&optional file)
  "Read the deck state from FILE (default `pi-deck-state-file'), or nil."
  (let ((file (or file pi-deck-state-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

;;;; Restoring

(defun pi-deck--restore-agent (record)
  "Restart the session or terminal described by RECORD unless it exists."
  (unless (get-buffer (plist-get record :buffer))
    (let ((dir (plist-get record :dir)))
      (when (file-directory-p dir)
        (condition-case err
            (let ((agent
                   (pcase (plist-get record :kind)
                     ('pi
                      ;; Pi writes a session file only once it has history;
                      ;; an agent without one restarts as a fresh session.
                      (let ((file (plist-get record :session-file)))
                        (pi-deck--track
                         (pilish-start-session dir (plist-get record :name)
                                               :args (and file (file-readable-p file)
                                                          (list "--session" file)))
                         'pi)))
                     ('shell
                      (pi-deck--track
                       (pi-deck-start-shell dir (string-trim (plist-get record :buffer) "\\*" "\\*")
                                            (plist-get record :command))
                       'shell)))))
              (when agent
                (setf (pi-deck-agent-assigned-space agent) (plist-get record :space)))
              agent)
          (error (message "pi-deck: could not restore %s: %s"
                          (plist-get record :buffer) (error-message-string err))
                 nil))))))

(defun pi-deck-apply-tabs (tabs &optional frame)
  "Recreate TABS in FRAME, skipping spaces that already have tabs there.
A lone unused initial tab is replaced."
  (with-selected-frame (or frame (selected-frame))
    (pi-deck-root-ensure)
    (let* ((initial (pi-deck--tabs))
           (lone (and (= (length initial) 2)
                      (cl-position-if-not #'pi-deck-root-tab-p initial)))
           (lone (and lone (null (pi-deck--tab-group (nth lone initial)))
                      (eq (car (nth lone initial)) 'current-tab)
                      ;; A single window besides the sidebar.
                      (= 1 (cl-count-if-not
                            (lambda (window)
                              (equal (buffer-name (window-buffer window))
                                     pi-deck-sidebar-buffer-name))
                            (window-list nil 'no-minibuf)))
                      (1+ lone)))
           (existing (delq nil (mapcar #'pi-deck--tab-group initial)))
           selected)
      ;; Window states name these buffers; create them before applying.
      (pi-deck-overview-buffer)
      (pi-deck-overview-buffer t)
      (dolist (tab tabs)
        (let ((group (plist-get tab :group)))
          (unless (or (plist-get tab :root) (and group (member group existing)))
            (pi-deck--new-tab-in-space (or group "") (and (plist-get tab :explicit)
                                                          (plist-get tab :name)))
            (when-let* ((ws (plist-get tab :ws)))
              (condition-case nil
                  (window-state-put ws (frame-root-window) 'safe)
                (error (delete-other-windows)))
              (when (window-parameter (selected-window) 'window-side)
                (select-window (or (get-mru-window nil nil t) (window-main-window)))))
            (when (plist-get tab :current)
              (setq selected (1+ (tab-bar--current-tab-index)))))))
      ;; LONE (a tab number) was the frame's unused initial tab; new
      ;; tabs were added after it.
      (when (and lone (> (length (pi-deck--tabs)) 2))
        (let ((tab-bar-tab-prevent-close-functions nil))
          (tab-bar-close-tab lone))
        (when (and selected (> selected lone)) (cl-decf selected)))
      (when selected (tab-bar-select-tab selected)))))

;;;###autoload
(defun pi-deck-restore-state (&optional file)
  "Restore spaces, sessions and tabs saved in FILE (default `pi-deck-state-file').
Pi sessions resume from their session files.  Tabs go to the selected client
frame, or to the next client frame when there is none."
  (interactive)
  (let ((state (or (pi-deck-read-state file)
                   (user-error "No saved pi-deck state"))))
    (dolist (space (plist-get state :spaces))
      (unless (pi-deck-space (plist-get space :name))
        (pi-deck--register-space (plist-get space :name) (plist-get space :dir))))
    (let ((agents (delq nil (mapcar #'pi-deck--restore-agent (plist-get state :agents)))))
      (setq pi-deck--sidebar-visible (plist-get state :sidebar))
      (let ((frame (pi-deck--state-frame)))
        (if frame
            (pi-deck-apply-tabs (plist-get state :tabs) frame)
          (setq pi-deck--pending-tabs (plist-get state :tabs))))
      (pi-deck--schedule-update)
      (message "pi-deck: restored %d spaces and %d sessions"
               (length (plist-get state :spaces)) (length agents))
      agents)))

(defun pi-deck-restore-if-pending ()
  "Restore the saved state after a daemon restart or with `pi-deck-auto-restore'."
  (let ((marker (pi-deck--restore-marker)))
    (when (or pi-deck-auto-restore (file-exists-p marker))
      (when (file-exists-p marker) (delete-file marker))
      (when (file-readable-p pi-deck-state-file)
        (pi-deck-restore-state)))))

;;;; Frames

(defun pi-deck-setup-frame (&optional frame)
  "Give the new client FRAME the deck's tabs."
  (let ((frame (or frame (selected-frame))))
    (when (memq frame (pi-deck--client-frames))
      (let ((source (cl-find-if (lambda (other) (and (not (eq other frame))
                                                     (cl-some #'pi-deck--tab-group
                                                              (pi-deck--tabs other))))
                                (pi-deck--client-frames))))
        (cond
         (pi-deck--pending-tabs
          (pi-deck-apply-tabs (prog1 pi-deck--pending-tabs (setq pi-deck--pending-tabs nil))
                              frame))
         ((and pi-deck-mirror-new-frames source)
          (pi-deck-apply-tabs (pi-deck-frame-tabs source) frame))
         ((and pi-deck-mirror-new-frames pi-deck--last-tabs)
          (pi-deck-apply-tabs pi-deck--last-tabs frame))
         (t (pi-deck-root-ensure frame)))))))

(defun pi-deck-remember-frame (frame)
  "Keep closing FRAME's tabs for the next client frame."
  (when (and (memq frame (pi-deck--client-frames))
             (cl-some #'pi-deck--tab-group (pi-deck--tabs frame)))
    (setq pi-deck--last-tabs (pi-deck-frame-tabs frame))))

(provide 'pi-deck-persist)
;;; pi-deck-persist.el ends here
