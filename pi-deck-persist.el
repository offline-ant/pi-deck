;;; pi-deck-persist.el --- Save and restore spaces, layouts and sessions -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The state file records registered spaces, each space's layout (a
;; writable window state), the current space, Pilish sessions with their
;; session files, terminals with their directory and command, and closed
;; workers.  Restoring resumes Pi sessions from those files and restarts
;; terminals; frames then show the current space with its layout.
;; Orchestration workers and panels belong to their parent Pi and are never
;; restored; closed workers return under their restored parent.
;;
;; The file belongs to one Emacs server, recorded by its socket.  That
;; server restores it whenever it starts, however it stopped, and is the
;; only Emacs that saves it automatically.  Others (a plain `emacs', a
;; disposable test daemon) neither restore nor overwrite it.
;;
;; A new client frame shows the space selected last, with its last known
;; layout (see `pi-deck--layouts').

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-sidebar)

(declare-function pi-deck--client-frames "pi-deck-daemon" ())
(declare-function pi-deck-server-socket "pi-deck-daemon" ())

(defcustom pi-deck-state-file (locate-user-emacs-file "var/pi-deck/state.eld")
  "File holding saved spaces, layouts and sessions."
  :type 'file
  :group 'pi-deck)

(defcustom pi-deck-auto-restore t
  "Whether the server owning the state file restores it when it starts.
Restoring resumes the Pi sessions and terminals open when Emacs stopped,
however it stopped.  When nil, the saved state stays untouched until the
first save, which first moves it to a \".bak\" file."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-restore-interval 0.15
  "Seconds between starting restored sessions and terminals; 0 starts all at once."
  :type 'number
  :group 'pi-deck)

(defcustom pi-deck-save-delay 2
  "Seconds after a change before the state is saved, or nil to save only on exit.
Changes are space edits, window layout changes, agents starting or
closing, and agents finishing a turn.  Nothing is saved periodically."
  :type '(choice (const nil) number)
  :group 'pi-deck)

(defvar pi-deck--state-owner nil
  "Server socket under which this Emacs owns `pi-deck-state-file', or nil.
Only the owner saves the state automatically.")

(defvar pi-deck--state-claimed nil
  "Non-nil once this Emacs decided whether it owns the state file.")

(defvar pi-deck--state-backup-pending nil
  "Non-nil while an owned but unrestored state file awaits its backup.")

(defvar pi-deck--restoring nil
  "Non-nil while a staggered restore starts sessions; its timer when waiting.")

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
               :command (buffer-local-value 'pi-deck--shell-command buffer)
               :space space))))))

(defun pi-deck--finished-worker-records ()
  "Return the closed workers as saved: parents become buffer names."
  (mapcar (lambda (record)
            (plist-put (copy-sequence record) :parent
                       (buffer-name (plist-get record :parent))))
          pi-deck--finished-workers))

(defun pi-deck-state ()
  "Return the current deck state.
The layouts on screen are remembered first, the selected frame's last."
  (let ((frames (pi-deck--client-frames)))
    (dolist (frame (append (remq (selected-frame) frames)
                           (and (memq (selected-frame) frames) (list (selected-frame)))))
      (pi-deck--remember-current-layout frame))
    (list :version 2
          :owner (or pi-deck--state-owner (pi-deck-server-socket))
          :spaces pi-deck-spaces
          :space pi-deck--last-space
          :layouts pi-deck--layouts
          :agents (let ((tab-index (pi-deck-tab-index)))
                    (delq nil (mapcar (lambda (agent) (pi-deck--agent-record agent tab-index))
                                      (pi-deck-agents))))
          :finished-workers (pi-deck--finished-worker-records))))

(defvar pi-deck--last-saved nil
  "Cons of the file and text last written by `pi-deck-save-state'.")

;;;###autoload
(defun pi-deck-save-state (&optional file)
  "Save spaces, layouts and sessions to FILE (default `pi-deck-state-file').
An unchanged state is not rewritten.  Saving the default file makes this
server its owner.  The first save over an owned state file that was not
restored keeps the old file as FILE.bak."
  (interactive)
  (when (member file (list nil pi-deck-state-file))
    (setq pi-deck--state-owner (or pi-deck--state-owner (pi-deck-server-socket))))
  (let* ((file (or file pi-deck-state-file))
         (state (pi-deck-state))
         (text (let ((print-length nil) (print-level nil) (print-circle nil))
                 (concat ";; pi-deck state -*- lisp-data -*-\n" (prin1-to-string state) "\n"))))
    (unless (and (equal pi-deck--last-saved (cons file text)) (file-exists-p file))
      (make-directory (file-name-directory file) t)
      (when (and pi-deck--state-backup-pending (equal file pi-deck-state-file))
        (setq pi-deck--state-backup-pending nil)
        (when (file-exists-p file) (rename-file file (concat file ".bak") t)))
      (with-file-modes #o600
        (write-region text nil file nil 'silent))
      (set-file-modes file #o600)
      (setq pi-deck--last-saved (cons file text)))
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

;;;; Saving after changes

(defvar pi-deck--save-timer nil
  "Pending state save after a change.")

(defun pi-deck-schedule-save (&rest _)
  "Save the state `pi-deck-save-delay' seconds after the first unsaved change.
Further changes before then join the same save, so one is never postponed
indefinitely by a stream of changes."
  (when (and pi-deck-save-delay (not pi-deck--save-timer))
    (setq pi-deck--save-timer (run-with-timer pi-deck-save-delay nil #'pi-deck--save-now))))

(defun pi-deck--save-now ()
  "Save the state when this Emacs owns the file and there is anything to save.
Nothing is saved during a restore: the file still lists what remains."
  (when pi-deck--save-timer
    (cancel-timer pi-deck--save-timer)
    (setq pi-deck--save-timer nil))
  (when (and pi-deck--state-owner
             (not pi-deck--restoring)
             (or pi-deck-spaces (> (hash-table-count pi-deck--agents) 0)))
    (with-demoted-errors "pi-deck save: %S" (pi-deck-save-state))))

(defun pi-deck--save-on-agent-change (_agent _old new)
  "Schedule a save when an agent finishes a turn or exits (becomes NEW)."
  (when (memq new '(idle unknown))
    (pi-deck-schedule-save)))

(defvar pi-deck--saved-structure nil
  "Spaces, agent buffers and closed workers when a save was last scheduled.")

(defun pi-deck--save-on-structure-change ()
  "Schedule a save when spaces, the set of agents or closed workers changed."
  (let ((structure (list (copy-tree pi-deck-spaces)
                         (mapcar #'pi-deck-agent-buffer (pi-deck-agents))
                         pi-deck--finished-workers)))
    (unless (equal structure pi-deck--saved-structure)
      (setq pi-deck--saved-structure structure)
      (pi-deck-schedule-save))))

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
                                               :session-file (and file (file-readable-p file)
                                                                  file))
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

;;;###autoload
(defun pi-deck-restore-state (&optional file)
  "Restore spaces, sessions and layouts saved in FILE.
FILE defaults to `pi-deck-state-file'.
Pi sessions resume from their session files, `pi-deck-restore-interval'
seconds apart.  Once all exist, client frames showing no space show the
saved current space; later frames show it too."
  (interactive)
  (when pi-deck--restoring
    (user-error "A pi-deck restore is still running"))
  (let ((state (or (pi-deck-read-state file)
                   (user-error "No saved pi-deck state"))))
    (dolist (space (plist-get state :spaces))
      (unless (pi-deck-space (plist-get space :name))
        (pi-deck--register-space (plist-get space :name) (plist-get space :dir))))
    (dolist (layout (plist-get state :layouts))
      (unless (assoc (car layout) pi-deck--layouts)
        (pi-deck--remember-layout (car layout) (cdr layout))))
    (when (pi-deck-space (plist-get state :space))
      (setq pi-deck--last-space (plist-get state :space)))
    (setq pi-deck--restoring t)
    (pi-deck--restore-next (plist-get state :agents) (length (plist-get state :spaces)) 0
                           (plist-get state :finished-workers))))

(defun pi-deck--restore-finished-workers (records)
  "Remember the saved closed workers RECORDS under their restored parents.
Records without a tracked parent or a readable session file are dropped,
and so are those already remembered."
  (let ((known (mapcar (lambda (record) (plist-get record :session-file))
                       pi-deck--finished-workers))
        restored)
    (dolist (record records)
      (let ((name (plist-get record :parent))
            (file (plist-get record :session-file)))
        (when-let* ((parent (and name (get-buffer name))))
          (when (and (pi-deck-agent parent) (file-readable-p file) (not (member file known)))
            (push (plist-put (copy-sequence record) :parent parent) restored)))))
    (setq pi-deck--finished-workers
          (append pi-deck--finished-workers (nreverse restored)))))

(defun pi-deck--restore-next (records spaces restored workers)
  "Restore RECORDS one by one; report SPACES and RESTORED sessions.
After each started record, the rest wait `pi-deck-restore-interval'.
Afterwards the closed WORKERS return under their parents and client frames
showing no space show the current one."
  (setq pi-deck--restoring t)
  (while (and records (eq pi-deck--restoring t))
    (when (pi-deck--restore-agent (pop records))
      (cl-incf restored)
      (when (and records (> pi-deck-restore-interval 0))
        (setq pi-deck--restoring
              (run-with-timer pi-deck-restore-interval nil #'pi-deck--restore-next
                              records spaces restored workers)))))
  (unless (timerp pi-deck--restoring)
    (setq pi-deck--restoring nil)
    (pi-deck--restore-finished-workers workers)
    ;; Layouts name the restored buffers: show them only now.
    (mapc #'pi-deck--open-last-space (pi-deck--client-frames))
    (pi-deck--schedule-update)
    (pi-deck-schedule-save)
    (message "pi-deck: restored %d spaces and %d sessions" spaces restored)))

(defun pi-deck-cancel-restore ()
  "Stop a running staggered restore; the state file keeps the rest."
  (when (timerp pi-deck--restoring) (cancel-timer pi-deck--restoring))
  (setq pi-deck--restoring nil))

(defun pi-deck-claim-state ()
  "Decide once whether this Emacs owns the state file, and restore it if so.
Only a running server can own it: the one whose socket the file records,
or any server when there is no file.  The owner restores the file when
`pi-deck-auto-restore' is non-nil.  `pi-deck-save-state' takes over a file
another server owns."
  (unless pi-deck--state-claimed
    (setq pi-deck--state-claimed t)
    (when-let* ((socket (pi-deck-server-socket)))
      (let* ((state (condition-case nil (pi-deck-read-state) (error 'unreadable)))
             (owner (and (consp state) (plist-get state :owner))))
        (cond
         ((null state) (setq pi-deck--state-owner socket))
         ((eq state 'unreadable)
          (setq pi-deck--state-owner socket
                pi-deck--state-backup-pending t)
          (message "pi-deck: cannot read %s; it is kept as .bak at the first save"
                   (abbreviate-file-name pi-deck-state-file)))
         ((equal owner socket)
          (setq pi-deck--state-owner socket)
          (if pi-deck-auto-restore
              (pi-deck-restore-state)
            (setq pi-deck--state-backup-pending t)))
         (t (message "pi-deck: %s belongs to %s; not restoring or saving it"
                     (abbreviate-file-name pi-deck-state-file)
                     (or owner "no server"))))))))

;;;; Frames

(defvar pi-deck--pending-reveal nil
  "Chat buffer the next client frame shows, opened while no frame existed.")

(defun pi-deck--recent-client-frame ()
  "Return the most recently used client frame, or nil."
  (car (sort (cl-remove-if #'frame-parent (pi-deck--client-frames))
             :key (lambda (frame) (window-use-time (get-mru-window frame t)))
             :reverse t)))

(defun pi-deck--open-last-space (frame)
  "Show the space selected last in FRAME when FRAME shows no space."
  (with-selected-frame frame
    (when (and (null (pi-deck-current-space)) (pi-deck-space pi-deck--last-space))
      (pi-deck-select-space pi-deck--last-space))
    (pi-deck--sidebar-sync)))

;;;###autoload
(defun pi-deck-show-session (chat)
  "Show the Pi session CHAT in the space for its directory; return its agent.
That is the closest space containing the directory, created when missing.
The most recently used client frame shows the session in that space and
gets focus; without a client frame, the next one shows it.  While
`pi-deck-mode' is on, this is `pi-orchestration-show-session-function',
used by `/switch emacs'."
  (let* ((agent (pi-deck--track chat 'pi))
         (dir (pi-deck-agent-directory agent)))
    (setf (pi-deck-agent-assigned-space agent)
          (or (pi-deck--space-for-dir dir)
              (let ((name (pi-deck--unique-space-name
                           (file-name-nondirectory (directory-file-name dir)))))
                (pi-deck--register-space name dir)
                name)))
    (if-let* ((frame (pi-deck--recent-client-frame)))
        (progn
          (select-frame-set-input-focus frame)
          (pi-deck-reveal agent))
      (setq pi-deck--pending-reveal chat))
    (pi-deck--schedule-update)
    agent))

(defun pi-deck-setup-frame (&optional frame)
  "Show the space selected last in the new client FRAME.
During a restore, the restore shows it once all sessions exist."
  (let ((frame (or frame (selected-frame))))
    (when (memq frame (pi-deck--client-frames))
      (unless pi-deck--restoring
        (pi-deck--open-last-space frame))
      (with-selected-frame frame
        (pi-deck--sidebar-sync)
        (when-let* ((chat (prog1 pi-deck--pending-reveal (setq pi-deck--pending-reveal nil)))
                    (agent (pi-deck-agent chat)))
          (pi-deck-reveal agent))))))

(defun pi-deck-remember-frame (frame)
  "Remember the layout closing FRAME shows for its space."
  (when (memq frame (pi-deck--client-frames))
    (pi-deck--remember-current-layout frame)))

(provide 'pi-deck-persist)
;;; pi-deck-persist.el ends here
