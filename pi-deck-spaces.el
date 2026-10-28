;;; pi-deck-spaces.el --- Spaces, tabs, panes and agent placement -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A space is a `tab-bar' tab group with a project directory; its tabs are
;; window layouts and its panes are windows showing Pilish chats (with their
;; input) or EAT terminals.  The space registry (name and directory) is
;; global; tabs belong to frames.  The pinned root space holds the overview.

;;; Code:

(require 'tab-bar)
(require 'project)
(require 'pi-deck-core)

(declare-function ghostel-create "ghostel" (&optional name display identity))
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function pi-deck-root-ensure "pi-deck-overview" (&optional frame))
(declare-function pi-deck-root-show "pi-deck-overview" ())
(declare-function pi-deck--sidebar-sync "pi-deck-overview" (&rest _))

(defcustom pi-deck-root-name "root"
  "Name of the pinned root space and its overview tab."
  :type 'string
  :group 'pi-deck)

(defcustom pi-deck-new-space-action 'agent
  "What a new space shows first: a Pi agent, a shell, or Dired."
  :type '(choice (const agent) (const shell) (const dired))
  :group 'pi-deck)

(defcustom pi-deck-reveal-action 'split
  "How to show an agent that no tab displays yet.
`split' splits the largest window of its space's tab, `window' replaces the
selected window, and `tab' opens a new tab in its space."
  :type '(choice (const split) (const window) (const tab))
  :group 'pi-deck)

(defcustom pi-deck-shell-program nil
  "Program for new shell panes; nil means ghostel's default shell.
That is `ghostel-shell' with ghostel's shell integration (OSC 133 prompt
marks for bash, zsh, fish and nushell).  An explicit program runs without
that integration, so its status needs its own prompt marks."
  :type '(choice (const nil) string)
  :group 'pi-deck)

(defvar pi-deck-spaces nil
  "Registered spaces: plists with :name and :dir, in creation order.")

(defvar-local pi-deck--shell-command nil
  "Program and arguments a pi-deck shell pane was started with.
Nil means ghostel's default shell.")

(defvar ghostel-kill-buffer-on-exit)

;;;; Registry

(defun pi-deck-space (name)
  "Return the registered space NAME, or nil."
  (cl-find name pi-deck-spaces :key (lambda (space) (plist-get space :name))
           :test #'equal))

(defun pi-deck-space-dir (name)
  "Return space NAME's directory, or nil."
  (plist-get (pi-deck-space name) :dir))

(defun pi-deck--register-space (name dir)
  "Register space NAME with directory DIR, replacing an older entry."
  (setq pi-deck-spaces
        (append (cl-remove name pi-deck-spaces
                           :key (lambda (space) (plist-get space :name)) :test #'equal)
                (list (list :name name :dir (and dir (file-name-as-directory
                                                      (expand-file-name dir))))))))

(defun pi-deck--tabs (&optional frame)
  "Return FRAME's tabs."
  (funcall tab-bar-tabs-function frame))

(defun pi-deck--tab-group (tab)
  "Return TAB's group (space) name, or nil."
  (alist-get 'group tab))

(defun pi-deck-space-names (&optional frame)
  "Return space names: the root, registered spaces, then FRAME's other groups."
  (let ((names (list pi-deck-root-name)))
    (dolist (space pi-deck-spaces)
      (cl-pushnew (plist-get space :name) names :test #'equal))
    (dolist (tab (pi-deck--tabs frame))
      (when-let* ((group (pi-deck--tab-group tab)))
        (cl-pushnew group names :test #'equal)))
    (nreverse names)))

(defun pi-deck-current-space (&optional frame)
  "Return the selected tab's space name in FRAME, or nil."
  (pi-deck--tab-group (tab-bar--current-tab-find nil frame)))

(defun pi-deck-root-tab-p (tab)
  "Return non-nil when TAB is the root overview tab."
  (equal (pi-deck--tab-group tab) pi-deck-root-name))

;;;; Git branch

(defvar pi-deck--branch-cache (make-hash-table :test 'equal)
  "Directory -> (TIME . BRANCH) cache.")

(defun pi-deck-git-branch (dir)
  "Return DIR's current Git branch, cached for a few seconds."
  (when (and dir (file-directory-p dir) (not (file-remote-p dir)))
    (let ((cached (gethash dir pi-deck--branch-cache)))
      (if (and cached (< (- (float-time) (car cached)) 5))
          (cdr cached)
        (let ((branch (with-temp-buffer
                        (let ((default-directory dir))
                          (when (eq 0 (ignore-errors
                                        (process-file "git" nil t nil
                                                      "branch" "--show-current")))
                            (let ((name (string-trim (buffer-string))))
                              (and (not (string-empty-p name)) name)))))))
          (puthash dir (cons (float-time) branch) pi-deck--branch-cache)
          branch)))))

;;;; Locations

(defun pi-deck--tab-buffers (tab frame)
  "Return the buffers shown by TAB of FRAME."
  (if (eq (car tab) 'current-tab)
      (mapcar #'window-buffer (window-list frame 'no-minibuf))
    (delq nil (mapcar (lambda (buffer) (if (bufferp buffer) buffer (get-buffer buffer)))
                      (ignore-errors (window-state-buffers (alist-get 'ws tab)))))))

(defun pi-deck-tab-index (&optional frame)
  "Return FRAME's tabs as a list of (TAB INDEX BUFFERS), INDEX 1-based."
  (let ((index 0))
    (mapcar (lambda (tab)
              (list tab (cl-incf index) (pi-deck--tab-buffers tab frame)))
            (pi-deck--tabs frame))))

(defun pi-deck--agent-tab (agent tab-index)
  "Return the first (TAB INDEX BUFFERS) of TAB-INDEX showing AGENT, or nil."
  (let ((buffers (pi-deck-agent-windows-buffers agent)))
    (cl-find-if (lambda (entry)
                  (cl-some (lambda (buffer) (memq buffer (nth 2 entry))) buffers))
                tab-index)))

(defun pi-deck--space-for-dir (dir)
  "Return the registered space whose directory contains DIR most closely."
  (when dir
    (let ((dir (file-name-as-directory (expand-file-name dir))) best)
      (dolist (space pi-deck-spaces)
        (let ((root (plist-get space :dir)))
          (when (and root (string-prefix-p root dir)
                     (or (not best) (> (length root) (length (plist-get best :dir)))))
            (setq best space))))
      (plist-get best :name))))

(defun pi-deck-agent-space (agent &optional tab-index seen)
  "Return AGENT's space name, or nil.
A displaying tab wins, then an explicit assignment, the parent's space, and
the closest space directory.  TAB-INDEX caches `pi-deck-tab-index'; SEEN
guards against parent cycles."
  (let ((tab-index (or tab-index (pi-deck-tab-index)))
        (parent (pi-deck-agent (pi-deck-agent-parent agent))))
    (or (pi-deck--tab-group (car (pi-deck--agent-tab agent tab-index)))
        (let ((space (pi-deck-agent-assigned-space agent)))
          (and space (member space (pi-deck-space-names)) space))
        (and parent (not (memq parent seen))
             (pi-deck-agent-space parent tab-index (cons agent seen)))
        (pi-deck--space-for-dir (pi-deck-agent-directory agent)))))

(defun pi-deck-tab-label (tab index)
  "Return a label for TAB at INDEX within its frame."
  (format "%d:%s" index (alist-get 'name tab)))

;;;; Tabs

(defun pi-deck--space-tab-entries (space &optional frame)
  "Return FRAME's (TAB INDEX) entries of SPACE in tab order."
  (let ((index 0) entries)
    (dolist (tab (pi-deck--tabs frame))
      (cl-incf index)
      (when (equal (pi-deck--tab-group tab) space)
        (push (list tab index) entries)))
    (nreverse entries)))

(defun pi-deck--set-tab-group (group &optional tab-number)
  "Put the current tab (or tab TAB-NUMBER) in GROUP without moving it.
pi-deck positions tabs itself; the default post-change hook would move the
tab next to existing members or to the end."
  (let ((tab-bar-tab-post-change-group-functions nil))
    (tab-bar-change-tab-group group tab-number)))

(defun pi-deck--select-tab-index (index)
  "Select the tab at 1-based INDEX."
  (tab-bar-select-tab index))

(defun pi-deck--new-tab-in-space (space &optional name)
  "Create and select a new tab in SPACE, placed after its last tab.
NAME, when non-nil, becomes the tab's explicit name."
  (when (window-parameter nil 'window-side)
    (select-window (get-mru-window nil nil t)))
  (let* ((entries (pi-deck--space-tab-entries space))
         (position (if entries (cadr (car (last entries))) (length (pi-deck--tabs)))))
    (let ((tab-bar-new-tab-group nil))
      (tab-bar-new-tab-to (1+ position)))
    (pi-deck--set-tab-group space)
    (when name (tab-bar-rename-tab name))
    (delete-other-windows)))

(defun pi-deck-select-space (space)
  "Select SPACE's most recently used tab, creating one when none exists."
  (let ((entries (pi-deck--space-tab-entries space)))
    (if (null entries)
        (pi-deck--new-tab-in-space space)
      (let ((recent (car (sort (copy-sequence entries)
                               :key (lambda (entry)
                                      (if (eq (car (car entry)) 'current-tab)
                                          most-positive-fixnum
                                        (or (alist-get 'time (car entry)) 0)))
                               :reverse t))))
        (pi-deck--select-tab-index (cadr recent))))))

(defun pi-deck--read-space (prompt &optional exclude-root extra)
  "Read a space name with PROMPT, showing status icons.
EXCLUDE-ROOT omits the root space; EXTRA choices are offered after the
spaces."
  (let* ((names (append (cl-remove-if (lambda (name) (and exclude-root (equal name pi-deck-root-name)))
                                      (pi-deck-space-names))
                        extra))
         (statuses (pi-deck-space-statuses))
         (annotate (lambda (name)
                     (unless (member name extra)
                       (concat " " (pi-deck-status-icon (or (cdr (assoc name statuses)) 'idle))
                               (when-let* ((dir (pi-deck-space-dir name)))
                                 (concat " " (abbreviate-file-name dir))))))))
    (completing-read prompt
                     (lambda (string pred action)
                       (if (eq action 'metadata)
                           `(metadata (category . pi-deck-space)
                                      (annotation-function . ,annotate)
                                      (display-sort-function . identity)
                                      (cycle-sort-function . identity))
                         (complete-with-action action names string pred)))
                     nil t)))

(defun pi-deck-space-statuses (&optional tab-index)
  "Return an alist of (SPACE . ROLLED-UP-STATUS) over current agents.
TAB-INDEX caches `pi-deck-tab-index'."
  (let ((tab-index (or tab-index (pi-deck-tab-index))) result)
    (dolist (agent (pi-deck-agents))
      (when-let* ((space (pi-deck-agent-space agent tab-index)))
        (let ((cell (assoc space result)))
          (if cell
              (setcdr cell (pi-deck-rollup (list (cdr cell) (pi-deck-agent-status agent))))
            (push (cons space (pi-deck-agent-status agent)) result)))))
    result))

(defun pi-deck-tab-status (tab &optional frame)
  "Return the rolled-up status of agents shown in TAB of FRAME, or nil."
  (let ((buffers (pi-deck--tab-buffers tab frame)))
    (pi-deck-rollup
     (delq nil (mapcar (lambda (agent)
                         (when (cl-some (lambda (buffer) (memq buffer buffers))
                                        (pi-deck-agent-windows-buffers agent))
                           (pi-deck-agent-status agent)))
                       (pi-deck-agents))))))

;;;; Space commands

(defun pi-deck--unique-space-name (base)
  "Return BASE, or BASE<N> when that space name is taken."
  (let ((name base) (n 1))
    (while (member name (pi-deck-space-names))
      (setq name (format "%s<%d>" base (cl-incf n))))
    name))

(defun pi-deck--read-project-dir ()
  "Read a project directory, offering known projects."
  (let ((projects (ignore-errors (project-known-project-roots))))
    (expand-file-name
     (if projects
         (completing-read "Space directory: " projects nil nil nil nil
                          (abbreviate-file-name default-directory))
       (read-directory-name "Space directory: ")))))

(defun pi-deck--unused-tab-p ()
  "Return non-nil when the current tab is ungrouped and shows only *scratch*."
  (and (null (pi-deck-current-space))
       (one-window-p)
       (equal (buffer-name (window-buffer)) "*scratch*")))

(defun pi-deck--make-space (dir &optional name)
  "Register a space for DIR named NAME and select its new, empty tab.
NAME defaults to DIR's base name; return the (unique) name used."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (name (pi-deck--unique-space-name
                (or (and name (not (string-empty-p name)) name)
                    (file-name-nondirectory (directory-file-name dir))))))
    (unless (file-directory-p dir) (user-error "Not a directory: %s" dir))
    (pi-deck--register-space name dir)
    (if (pi-deck--unused-tab-p)
        (pi-deck--set-tab-group name)
      (pi-deck--new-tab-in-space name))
    (pi-deck--sidebar-sync)
    name))

;;;###autoload
(defun pi-deck-create-space (dir &optional name)
  "Create a space for project DIR named NAME and show its first pane.
NAME defaults to DIR's base name.  The first pane follows
`pi-deck-new-space-action'."
  (interactive
   (let ((dir (pi-deck--read-project-dir)))
     (list dir (read-string "Space name: "
                            (pi-deck--unique-space-name
                             (file-name-nondirectory (directory-file-name dir)))))))
  (let ((name (pi-deck--make-space dir name)))
    (pcase pi-deck-new-space-action
      ('agent (pi-deck-new-agent nil))
      ('shell (pi-deck-new-shell))
      (_ (dired dir)))
    (pi-deck--schedule-update)
    name))

;;;###autoload
(defun pi-deck-switch-space (space)
  "Switch to SPACE's most recent tab."
  (interactive (list (pi-deck--read-space "Switch to space: ")))
  (if (equal space pi-deck-root-name)
      (pi-deck-root-show)
    (pi-deck-select-space space)))

(defun pi-deck--require-space ()
  "Return the current non-root space or signal a user error."
  (let ((space (pi-deck-current-space)))
    (when (or (null space) (equal space pi-deck-root-name))
      (user-error "Not in a space; use `pi-deck-create-space' or `pi-deck-switch-space'"))
    space))

(defun pi-deck-rename-space (new-name)
  "Rename the current space to NEW-NAME."
  (interactive
   (list (read-string (format "Rename space %s to: " (pi-deck--require-space)))))
  (let ((old (pi-deck--require-space)))
    (when (member new-name (pi-deck-space-names))
      (user-error "Space %s already exists" new-name))
    (dolist (frame (frame-list))
      (dolist (entry (pi-deck--space-tab-entries old frame))
        (with-selected-frame frame
          (pi-deck--set-tab-group new-name (cadr entry)))))
    (dolist (space pi-deck-spaces)
      (when (equal (plist-get space :name) old)
        (plist-put space :name new-name)))
    (maphash (lambda (_buffer agent)
               (when (equal (pi-deck-agent-assigned-space agent) old)
                 (setf (pi-deck-agent-assigned-space agent) new-name)))
             pi-deck--agents)
    (pi-deck--schedule-update)))

(defun pi-deck--space-agents (space)
  "Return agents whose space is SPACE."
  (let ((tab-index (pi-deck-tab-index)))
    (cl-remove-if-not (lambda (agent) (equal (pi-deck-agent-space agent tab-index) space))
                      (pi-deck-agents))))

(defun pi-deck-close-space (space &optional stop-agents)
  "Close SPACE's tabs in all frames and forget it.
With prefix argument STOP-AGENTS, also stop the space's agents."
  (interactive (list (pi-deck--read-space "Close space: " t) current-prefix-arg))
  (when (equal space pi-deck-root-name)
    (user-error "The root space cannot be closed"))
  (let ((agents (and stop-agents (pi-deck--space-agents space))))
    (when (or (not (called-interactively-p 'any))
              (yes-or-no-p (format "Close space %s%s? " space
                                   (if agents (format " and stop %d agents" (length agents)) ""))))
      (dolist (agent agents) (pi-deck-stop-agent agent))
      (dolist (frame (frame-list))
        (with-selected-frame frame
          (when (pi-deck--space-tab-entries space)
            (when (= (length (pi-deck--space-tab-entries space)) (length (pi-deck--tabs)))
              (pi-deck-root-ensure))
            (let ((tab-bar-close-last-tab-choice 'delete-frame))
              (tab-bar-close-group-tabs space)))))
      (setq pi-deck-spaces
            (cl-remove space pi-deck-spaces :key (lambda (s) (plist-get s :name)) :test #'equal))
      (pi-deck--schedule-update))))

;;;; Tab commands

(defun pi-deck-new-tab (&optional name)
  "Open a new tab named NAME in the current space."
  (interactive (list (let ((name (read-string "Tab name (empty for automatic): ")))
                       (and (not (string-empty-p name)) name))))
  (let ((space (pi-deck--require-space))
        (buffer (current-buffer)))
    (pi-deck--new-tab-in-space space name)
    (switch-to-buffer buffer)))

(defun pi-deck--cycle-tab (step)
  "Select the tab STEP positions away within the current space."
  (let* ((space (pi-deck-current-space))
         (entries (pi-deck--space-tab-entries space))
         (position (cl-position-if (lambda (entry) (eq (car (car entry)) 'current-tab))
                                   entries)))
    (when (and position (> (length entries) 1))
      (pi-deck--select-tab-index
       (cadr (nth (mod (+ position step) (length entries)) entries))))))

(defun pi-deck-next-tab ()
  "Select the next tab of the current space, wrapping around."
  (interactive)
  (pi-deck--cycle-tab 1))

(defun pi-deck-previous-tab ()
  "Select the previous tab of the current space, wrapping around."
  (interactive)
  (pi-deck--cycle-tab -1))

(defun pi-deck-select-tab (n)
  "Select the Nth tab (1-based) of the current space."
  (interactive (list (if (integerp current-prefix-arg) current-prefix-arg
                       (let ((key (event-basic-type last-command-event)))
                         (if (and (characterp key) (<= ?1 key ?9)) (- key ?0)
                           (read-number "Tab number: "))))))
  (let ((entry (nth (1- n) (pi-deck--space-tab-entries (pi-deck-current-space)))))
    (unless entry (user-error "No tab %d in this space" n))
    (pi-deck--select-tab-index (cadr entry))))

(defun pi-deck-rename-tab (name)
  "Rename the current tab to NAME."
  (interactive (list (read-string "Tab name: ")))
  (tab-bar-rename-tab name))

(defun pi-deck-close-tab ()
  "Close the current tab; the root tab is pinned."
  (interactive)
  (when (pi-deck-root-tab-p (tab-bar--current-tab-find))
    (user-error "The root tab is pinned"))
  (tab-bar-close-tab))

;;;; Panes

(defun pi-deck--display-agent-in-window (agent window)
  "Show AGENT in WINDOW: a chat with its input window below, or a terminal."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (set-window-buffer window buffer)
    (when (eq (pi-deck-agent-kind agent) 'pi)
      (let ((input (pilish-session-input-buffer buffer)))
        (when (and (buffer-live-p input) (not (get-buffer-window input)))
          ;; Pilish sizes the input (`pilish-input-window-height', e.g. `fit'
          ;; to the draft); resizing the pane (splits, balancing) keeps it.
          (when-let* ((input-window (pilish-split-input-window window input)))
            (window-preserve-size input-window nil t))
          (with-selected-window window (goto-char (point-max))))))
    window))

(defun pi-deck--pane-window ()
  "Return the selected pane's window, freed of a chat's input window.
When the selected window belongs to a Pilish session, its input window is
deleted and the chat window returned, so the whole pane can be reused."
  (let* ((window (selected-window))
         (chat (pilish-session-chat-buffer (window-buffer window))))
    (if (not chat)
        window
      (let ((chat-window (get-buffer-window chat))
            (input-window (get-buffer-window (pilish-session-input-buffer chat))))
        (when (and input-window chat-window (not (eq input-window chat-window)))
          (delete-window input-window))
        (select-window (or chat-window (selected-window)))))))

(defun pi-deck--pane-of (window)
  "Return the window holding WINDOW's whole pane.
A Pilish chat window and its input window stacked together form one pane,
held by their parent window; any other WINDOW is its own pane."
  (let* ((chat (pilish-session-chat-buffer (window-buffer window)))
         (chat-window (and chat (get-buffer-window chat)))
         (input-window (and chat (get-buffer-window (pilish-session-input-buffer chat))))
         (parent (and chat-window input-window (window-parent chat-window))))
    (if (and parent
             (memq window (list chat-window input-window))
             (eq parent (window-parent input-window))
             (= (window-child-count parent) 2))
        parent
      window)))

(defun pi-deck--split-pane (window direction)
  "Split the pane holding WINDOW in DIRECTION (`right' or `below') in half.
Return the new live window.  Other panes keep their sizes, and a chat's
input window stays attached to its chat."
  (split-window (pi-deck--pane-of window) nil direction))

(defun pi-deck--input-or-chat-window (agent)
  "Return the window to focus for AGENT: its input when shown, else its buffer."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (or (and (eq (pi-deck-agent-kind agent) 'pi)
             (get-buffer-window (pilish-session-input-buffer buffer)))
        (get-buffer-window buffer))))

(defun pi-deck-reveal (agent &optional action)
  "Show AGENT in the selected frame, focusing it.
Select the tab already showing it; otherwise use ACTION (default
`pi-deck-reveal-action') in its space."
  (let* ((tab-index (pi-deck-tab-index))
         (entry (pi-deck--agent-tab agent tab-index))
         (space (pi-deck-agent-space agent tab-index)))
    (if entry
        (pi-deck--select-tab-index (nth 1 entry))
      (when (and space (not (equal space (pi-deck-current-space))))
        (pi-deck-select-space space))
      (when (pi-deck-root-tab-p (tab-bar--current-tab-find))
        (pi-deck--new-tab-in-space (or space "other")))
      (pcase (or action pi-deck-reveal-action)
        ('tab (pi-deck--new-tab-in-space (pi-deck-current-space) (pi-deck-agent-name agent))
              (pi-deck--display-agent-in-window agent (selected-window)))
        ('window (pi-deck--display-agent-in-window agent (pi-deck--pane-window)))
        (_ (let ((largest (get-largest-window)))
             (pi-deck--display-agent-in-window
              agent (pi-deck--split-pane largest (if (> (window-width largest) 160)
                                                     'right 'below)))))))
    (when-let* ((window (pi-deck--input-or-chat-window agent)))
      (select-window window))
    (pi-deck-mark-seen agent)))

(defun pi-deck--current-space-dir ()
  "Return the current space's directory, or `default-directory'."
  (or (pi-deck-space-dir (pi-deck-current-space)) default-directory))

(defconst pi-deck--new-space-choice "New space…"
  "Space picker entry that creates a space.")

(defun pi-deck--read-space-or-new (prompt)
  "Pick an existing space or `pi-deck--new-space-choice' with PROMPT.
Return the chosen space name; a new space is created with an empty tab."
  (let ((choice (pi-deck--read-space prompt t (list pi-deck--new-space-choice))))
    (if (equal choice pi-deck--new-space-choice)
        (let ((dir (pi-deck--read-project-dir)))
          (pi-deck--make-space dir (read-string "Space name: "
                                                (pi-deck--unique-space-name
                                                 (file-name-nondirectory
                                                  (directory-file-name dir))))))
      choice)))

(defun pi-deck--enter-pane-space ()
  "Make sure the selected tab belongs to a space that can hold panes.
On the root tab or an ungrouped tab, pick a space (or create one) and
switch to it."
  (let ((space (pi-deck-current-space)))
    (when (or (null space) (equal space pi-deck-root-name))
      (let ((choice (pi-deck--read-space-or-new "Space: ")))
        (unless (equal (pi-deck-current-space) choice)
          (pi-deck-select-space choice))))))

(defun pi-deck--unique-session-name (dir)
  "Return the next free numeric Pilish session name in DIR."
  (let ((n 1))
    (while (or (pilish--find-session (file-name-as-directory dir) (number-to-string n))
               (get-buffer (pilish--buffer-name :chat (file-name-as-directory dir)
                                                (number-to-string n))))
      (cl-incf n))
    (number-to-string n)))

(defun pi-deck-new-agent (name &optional dir args)
  "Start a Pi session NAME in the current space and show it in this window.
DIR defaults to the space's directory; ARGS are extra Pi arguments."
  (interactive (progn
                 (pi-deck--enter-pane-space)
                 (list (let ((name (read-string "Agent name (empty for next number): ")))
                         (and (not (string-empty-p name)) name)))))
  (let* ((dir (file-name-as-directory (expand-file-name (or dir (pi-deck--current-space-dir)))))
         (name (or name (pi-deck--unique-session-name dir)))
         (chat (pilish-start-session dir name :args args))
         (agent (pi-deck--track chat 'pi)))
    (setf (pi-deck-agent-assigned-space agent) (pi-deck-current-space))
    (unless (pi-deck-root-tab-p (tab-bar--current-tab-find))
      (pi-deck--display-agent-in-window agent (pi-deck--pane-window))
      (select-window (pi-deck--input-or-chat-window agent)))
    agent))

(defun pi-deck-start-shell (dir &optional name command)
  "Start a ghostel terminal in DIR and return its buffer.
COMMAND is a program with arguments (a list), else `pi-deck-shell-program'
or ghostel's default shell.  NAME is the buffer name without stars.  The
buffer stays after its process exits, so its output and status remain."
  (require 'ghostel)
  (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
         (command (or command (and pi-deck-shell-program (list pi-deck-shell-program))))
         (name (generate-new-buffer-name (format "*%s*" (or name "shell"))))
         (buffer (if command
                     (let ((buffer (generate-new-buffer name)))
                       (ghostel-exec buffer (car command) (cdr command))
                       buffer)
                   (ghostel-create name))))
    (with-current-buffer buffer
      (setq-local ghostel-kill-buffer-on-exit nil)
      (setq pi-deck--shell-command command))
    buffer))

(defun pi-deck-new-shell (&optional split)
  "Open a shell pane in the current space's directory.
With SPLIT (prefix argument), show it in a new window to the right."
  (interactive (progn (pi-deck--enter-pane-space) (list current-prefix-arg)))
  (let* ((space (pi-deck-current-space))
         (buffer (pi-deck-start-shell (pi-deck--current-space-dir)
                                      (format "shell:%s" (or space "shell"))))
         (agent (pi-deck--track buffer 'shell))
         (window (if split (split-window (pi-deck--pane-window) nil 'right)
                   (pi-deck--pane-window))))
    (setf (pi-deck-agent-assigned-space agent) space)
    (set-window-buffer window buffer)
    (select-window window)
    agent))

(defun pi-deck-split-right ()
  "Split the selected pane (a chat with its input counts as one) to the right."
  (interactive)
  (select-window (pi-deck--split-pane (selected-window) 'right)))

(defun pi-deck-split-below ()
  "Split the selected pane (a chat with its input counts as one) below."
  (interactive)
  (select-window (pi-deck--split-pane (selected-window) 'below)))

(defun pi-deck-zoom ()
  "Toggle zooming the selected pane within the current tab."
  (interactive)
  (let* ((tab (tab-bar--current-tab-find))
         (saved (alist-get 'pi-deck-zoom (cdr tab))))
    (if (and saved (eq (pi-deck--pane-of (selected-window)) (window-main-window)))
        (progn (set-window-configuration saved)
               (setf (alist-get 'pi-deck-zoom (cdr tab) nil 'remove) nil))
      (setf (alist-get 'pi-deck-zoom (cdr tab)) (current-window-configuration))
      (delete-other-windows (pi-deck--pane-of (selected-window))))))

(defun pi-deck-close-pane ()
  "Close the selected pane; a chat pane also closes its input window."
  (interactive)
  (let* ((chat (pilish-session-chat-buffer (window-buffer)))
         (windows (if chat
                      (append (get-buffer-window-list chat nil)
                              (get-buffer-window-list (pilish-session-input-buffer chat) nil))
                    (list (selected-window)))))
    (dolist (window windows)
      (if (one-window-p) (switch-to-prev-buffer window) (delete-window window)))))

(defun pi-deck-stop-agent (agent)
  "Stop AGENT and kill its buffers without confirmation."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (if (eq (pi-deck-agent-kind agent) 'pi)
        (pilish-close-session buffer)
      (let ((kill-buffer-query-functions nil))
        (when-let* ((process (get-buffer-process buffer)))
          (set-process-query-on-exit-flag process nil))
        (kill-buffer buffer)))
    (remhash buffer pi-deck--agents)
    (pi-deck--schedule-update)))

(provide 'pi-deck-spaces)
;;; pi-deck-spaces.el ends here
