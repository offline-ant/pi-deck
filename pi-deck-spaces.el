;;; pi-deck-spaces.el --- Spaces, layouts, panes and agent placement -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; A space is a project directory with one window layout, whose panes are
;; windows showing Pilish chats (with their input) or ghostel terminals.
;; The space registry (name and directory) is global.  Each frame keeps a
;; space's live layout in a hidden `tab-bar' tab whose group is the space
;; name (`tab-bar-show' is nil while `pi-deck-mode' is on), so switching
;; spaces switches tabs.  `pi-deck--layouts' keeps the last layout of every
;; space, for frames that open it for the first time and for the state
;; file.  Hiding panes, stopping agents and closing spaces live in
;; `pi-deck-close'.

;;; Code:

(require 'tab-bar)
(require 'project)
(require 'pi-deck-core)

(declare-function ghostel-create "ghostel" (&optional name display identity))
(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(declare-function pi-deck--sidebar-sync "pi-deck-sidebar" (&rest _))

(defcustom pi-deck-new-space-action 'agent
  "What a new space shows first: a Pi agent, a shell, or Dired."
  :type '(choice (const agent) (const shell) (const dired))
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

(defvar pi-deck--layouts nil
  "Alist of space names and their last known writable window states.
Updated when a frame leaves a space and when the state is saved; a frame
opening a space it has no tab for starts from it.")

(defvar pi-deck--last-space nil
  "Space selected most recently in any frame; new frames open it.")

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

(defun pi-deck-space-names ()
  "Return the registered space names in creation order."
  (mapcar (lambda (space) (plist-get space :name)) pi-deck-spaces))

(defun pi-deck-current-space (&optional frame)
  "Return the space FRAME shows, or nil."
  (pi-deck--tab-group (tab-bar--current-tab-find nil frame)))

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
  "Return AGENT's home space name, or nil.
That is its explicit assignment, else its parent's space, else the closest
space directory.  Only an agent none of these place belongs to a space
whose layout displays it: showing an agent in another space never moves
it.  TAB-INDEX caches `pi-deck-tab-index'; SEEN guards against parent
cycles."
  (let ((parent (pi-deck-agent (pi-deck-agent-parent agent))))
    (or (let ((space (pi-deck-agent-assigned-space agent)))
          (and space (member space (pi-deck-space-names)) space))
        (and parent (not (memq parent seen))
             (pi-deck-agent-space parent tab-index (cons agent seen)))
        (pi-deck--space-for-dir (pi-deck-agent-directory agent))
        (pi-deck--tab-group (car (pi-deck--agent-tab agent (or tab-index (pi-deck-tab-index))))))))

;;;; Layouts

(defun pi-deck--space-tab-number (space &optional frame)
  "Return the 1-based number of FRAME's tab holding SPACE's layout, or nil."
  (let ((position (cl-position space (pi-deck--tabs frame)
                               :key #'pi-deck--tab-group :test #'equal)))
    (and position (1+ position))))

(defun pi-deck--set-tab-group (group &optional tab-number)
  "Put the current tab (or tab TAB-NUMBER) in GROUP without moving it.
Tabs are hidden, so tab-bar's echo message is suppressed."
  (let ((tab-bar-tab-post-change-group-functions nil)
        (inhibit-message t)
        (message-log-max nil))
    (tab-bar-change-tab-group group tab-number)))

(defun pi-deck--space-start-buffer (space)
  "Return the buffer a new layout of SPACE shows: Dired of its directory."
  (let ((dir (pi-deck-space-dir space)))
    (if (and dir (file-directory-p dir))
        (dired-noselect dir)
      (get-scratch-buffer-create))))

(defun pi-deck--remember-layout (space window-state)
  "Record WINDOW-STATE as SPACE's last known layout."
  (when (and space window-state (pi-deck-space space))
    (setf (alist-get space pi-deck--layouts nil nil #'equal) window-state)))

(defun pi-deck--remember-current-layout (&optional frame)
  "Record the layout FRAME shows as its space's last known layout.
Side windows such as the sidebar are not part of it."
  (let ((frame (or frame (selected-frame))))
    (pi-deck--remember-layout (pi-deck-current-space frame)
                              (window-state-get (window-main-window frame) t))))

(defun pi-deck--apply-layout (space)
  "Show SPACE's last known layout in the selected frame's current tab.
Without one, the tab shows the space's directory in a single pane."
  (pi-deck--single-window)
  (switch-to-buffer (pi-deck--space-start-buffer space) t t)
  (when-let* ((state (alist-get space pi-deck--layouts nil nil #'equal)))
    (condition-case nil
        (window-state-put state (window-main-window) 'safe)
      (error (pi-deck--single-window)))
    (pi-deck--select-ordinary-window)
    ;; Window states keep no atoms: pair chats and inputs again.
    (pilish-repair-window-pairs)))

(defun pi-deck-select-space (space)
  "Show SPACE's layout in the selected frame.
A frame without a tab for SPACE gets one with the space's last known
layout; an ungrouped current tab (a new frame's first) is reused for it."
  (let ((number (pi-deck--space-tab-number space)))
    (cond
     (number (unless (equal space (pi-deck-current-space))
               (pi-deck--remember-current-layout)
               (tab-bar-select-tab number)))
     ((null (pi-deck-current-space))
      (pi-deck--set-tab-group space)
      (pi-deck--apply-layout space)
      (pi-deck--on-space-shown))
     (t
      (pi-deck--select-ordinary-window)
      (pi-deck--remember-current-layout)
      (let ((tab-bar-new-tab-group nil)
            (tab-bar-new-tab-choice #'get-scratch-buffer-create)
            ;; Tabs are hidden: no "Added new tab" message.
            (inhibit-message t)
            (message-log-max nil))
        (tab-bar-new-tab-to (1+ (length (pi-deck--tabs)))))
      (pi-deck--set-tab-group space)
      (pi-deck--apply-layout space)
      (pi-deck--on-space-shown))))
  (setq pi-deck--last-space space))

(defun pi-deck--on-space-shown ()
  "Update views after the selected frame started showing another space."
  (setq pi-deck--last-space (or (pi-deck-current-space) pi-deck--last-space))
  (pi-deck--sidebar-sync)
  (pi-deck-mark-visible-seen)
  (pi-deck--schedule-update))

(defun pi-deck--read-space (prompt &optional extra)
  "Read a space name with PROMPT, showing status icons.
EXTRA choices are offered after the spaces."
  (let* ((names (append (pi-deck-space-names) extra))
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

;;;; Space commands

(defun pi-deck--unique-space-name (base)
  "Return BASE, or BASE<N> when that space name is taken."
  (let ((name base) (n 1))
    (while (member name (pi-deck-space-names))
      (setq name (format "%s<%d>" base (cl-incf n))))
    name))

(defun pi-deck--project-dir-table (projects base)
  "Return a completion table of PROJECTS for reading a space directory.
When the input names an existing directory that is not one of PROJECTS,
the input itself is listed first, annotated \"typed\", so accepting the
first candidate takes exactly what was typed.  Relative input is checked
against BASE.  Candidates keep their order (project.el's most recent
first).  The input is read from the minibuffer when the table is called
there: completion styles such as `substring' and `orderless' call the
table with an empty string and filter afterwards."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . identity)
                   (cycle-sort-function . identity)
                   (annotation-function
                    . ,(lambda (candidate)
                         (unless (member candidate projects) "  (typed)"))))
      (let ((input (if (minibufferp) (minibuffer-contents-no-properties) string)))
        (complete-with-action
         action
         (if (and (not (string-empty-p input))
                  (not (member (file-name-as-directory input) projects))
                  (file-directory-p (expand-file-name input base)))
             (cons input projects)
           projects)
         string pred)))))

(defun pi-deck--read-project-dir ()
  "Read a project directory, offering known projects.
An existing directory typed at the prompt is offered first (see
`pi-deck--project-dir-table'); empty input takes `default-directory'."
  (let ((projects (ignore-errors (project-known-project-roots))))
    (expand-file-name
     (if projects
         (completing-read "Space directory: "
                          (pi-deck--project-dir-table projects default-directory)
                          nil nil nil nil (abbreviate-file-name default-directory))
       (read-directory-name "Space directory: ")))))

(defun pi-deck--make-space (dir &optional name)
  "Register a space for DIR named NAME and show its new layout.
NAME defaults to DIR's base name; return the (unique) name used."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (name (pi-deck--unique-space-name
                (or (and name (not (string-empty-p name)) name)
                    (file-name-nondirectory (directory-file-name dir))))))
    (unless (file-directory-p dir) (user-error "Not a directory: %s" dir))
    (pi-deck--register-space name dir)
    (pi-deck-select-space name)
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
  "Show SPACE's layout."
  (interactive (list (pi-deck--read-space "Switch to space: ")))
  (pi-deck-select-space space))

(defun pi-deck--require-space ()
  "Return the current space or signal a user error."
  (or (pi-deck-current-space)
      (user-error "Not in a space; use `pi-deck-create-space' or `pi-deck-switch-space'")))

(defun pi-deck-rename-space (new-name)
  "Rename the current space to NEW-NAME."
  (interactive
   (list (read-string (format "Rename space %s to: " (pi-deck--require-space)))))
  (let ((old (pi-deck--require-space)))
    (when (member new-name (pi-deck-space-names))
      (user-error "Space %s already exists" new-name))
    (dolist (frame (frame-list))
      (when-let* ((number (pi-deck--space-tab-number old frame)))
        (with-selected-frame frame
          (pi-deck--set-tab-group new-name number))))
    (dolist (space pi-deck-spaces)
      (when (equal (plist-get space :name) old)
        (plist-put space :name new-name)))
    (when-let* ((cell (assoc old pi-deck--layouts)))
      (setcar cell new-name))
    (when (equal pi-deck--last-space old) (setq pi-deck--last-space new-name))
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

;;;; Panes

(defun pi-deck--display-agent-in-window (agent window)
  "Show AGENT in WINDOW and return the window now showing its buffer.
A Pi agent's chat gets its input window below it as one Pilish pair (see
`pilish-window-pair'); when WINDOW is half of a pair, the pair switches
to AGENT.  A terminal takes the place of WINDOW's whole pair."
  (let ((buffer (pi-deck-agent-buffer agent))
        (pair (pilish-window-pair window)))
    (when pair (setq window (car pair)))
    (if (eq (pi-deck-agent-kind agent) 'pi)
        (let ((input (pilish-session-input-buffer buffer)))
          (set-window-buffer window buffer)
          (when (buffer-live-p input)
            (pilish-split-input-window window input))
          (with-selected-window window (goto-char (point-max))))
      (set-window-buffer (pilish-dissolve-window-pair window) buffer))
    window))

(defun pi-deck--pane-window ()
  "Return the selected pane's window: a pair's chat window, else the window.
From a side window such as the sidebar, the pane is the main-area window
the user came from (see `pi-deck--main-window')."
  (let ((window (pi-deck--main-window)))
    (car (or (pilish-window-pair window) (list window)))))

(defun pi-deck--pane-of (window)
  "Return the window holding WINDOW's whole pane.
A Pilish chat and input pair is one pane, held by its parent window; any
other WINDOW is its own pane."
  (if-let* ((pair (pilish-window-pair window)))
      (window-parent (car pair))
    window))

(defun pi-deck--beside-direction (window)
  "Return where a new pane beside WINDOW's pane goes: `right' when wide."
  (if (>= (window-total-width (pi-deck--pane-of window)) 120) 'right 'below))

(defun pi-deck--show-beside (agent window)
  "Show AGENT in a new pane beside WINDOW's pane and return its window."
  (pi-deck--display-agent-in-window
   agent (pi-deck--split-pane window (pi-deck--beside-direction window))))

(defun pi-deck--split-pane (window direction)
  "Split the pane holding WINDOW in DIRECTION (`right' or `below') in half.
Return the new live window.  Other panes keep their sizes, and a chat's
input window stays attached to its chat.  The new window shows WINDOW's
buffer, even when a side window such as the sidebar is selected."
  (with-selected-window window
    (split-window (pi-deck--pane-of window) nil direction)))

(defun pi-deck--input-or-chat-window (agent)
  "Return the window to focus for AGENT: its input when shown, else its buffer."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (or (and (eq (pi-deck-agent-kind agent) 'pi)
             (get-buffer-window (pilish-session-input-buffer buffer)))
        (get-buffer-window buffer))))

(defun pi-deck-reveal (agent)
  "Show AGENT in the selected frame and focus it.
An agent on screen is just focused.  Otherwise its space's layout is shown,
and if that does not show it either, a worker or panel whose parent is on
screen opens beside the parent; any other agent replaces the most recently
used pane.  An agent without a space shows in the current layout."
  (unless (pi-deck--input-or-chat-window agent)
    (let ((space (pi-deck-agent-space agent)))
      (when (and space (not (equal space (pi-deck-current-space))))
        (pi-deck-select-space space)))
    (unless (pi-deck--input-or-chat-window agent)
      (let ((parent (and (pi-deck-agent-worker-p agent)
                         (pi-deck-agent (pi-deck-agent-parent agent)))))
        (if-let* ((parent-window (and parent (pi-deck--input-or-chat-window parent))))
            (pi-deck--show-beside agent parent-window)
          (pi-deck--display-agent-in-window agent (pi-deck--pane-window))))))
  (when-let* ((window (pi-deck--input-or-chat-window agent)))
    (select-window window))
  (pi-deck-mark-seen agent))

(defun pi-deck-show-beside (agent)
  "Show AGENT in a new pane beside the current pane, keeping focus.
An agent on screen stays where it is."
  (unless (pi-deck--input-or-chat-window agent)
    (save-selected-window
      (pi-deck--show-beside agent (pi-deck--pane-window))))
  (pi-deck-mark-seen agent))

(defun pi-deck--current-space-dir ()
  "Return the current space's directory, or `default-directory'."
  (or (pi-deck-space-dir (pi-deck-current-space)) default-directory))

(defconst pi-deck--new-space-choice "New space…"
  "Space picker entry that creates a space.")

(defun pi-deck--read-space-or-new (prompt)
  "Pick an existing space or `pi-deck--new-space-choice' with PROMPT.
Return the chosen space name; a new space is created with an empty layout."
  (let ((choice (pi-deck--read-space prompt (list pi-deck--new-space-choice))))
    (if (equal choice pi-deck--new-space-choice)
        (let ((dir (pi-deck--read-project-dir)))
          (pi-deck--make-space dir (read-string "Space name: "
                                                (pi-deck--unique-space-name
                                                 (file-name-nondirectory
                                                  (directory-file-name dir))))))
      choice)))

(defun pi-deck--enter-pane-space ()
  "Make sure the selected frame shows a space.
Outside a space, pick a space (or create one) and switch to it."
  (unless (pi-deck-current-space)
    (let ((choice (pi-deck--read-space-or-new "Space: ")))
      (unless (equal (pi-deck-current-space) choice)
        (pi-deck-select-space choice)))))

(defun pi-deck--unique-session-name (dir)
  "Return the next free numeric Pilish session name in DIR."
  (let ((n 1))
    (while (pilish-session-exists-p dir (number-to-string n))
      (cl-incf n))
    (number-to-string n)))

(defun pi-deck--new-pane-window ()
  "Return the window a new agent of the current layout goes to.
That is a new pane beside the selected one when the layout shows an agent,
else the selected pane."
  (if (cl-some (lambda (window)
                 (let ((buffer (window-buffer window)))
                   (pi-deck-agent (or (pilish-session-chat-buffer buffer) buffer))))
               (window-list nil 'no-minibuf))
      (let ((window (pi-deck--pane-window)))
        (pi-deck--split-pane window (pi-deck--beside-direction window)))
    (pi-deck--pane-window)))

(defun pi-deck-new-agent (name &optional dir args)
  "Start a Pi session NAME in the current space and show it in a new pane.
The first agent of a layout takes its pane.  DIR defaults to the space's
directory; ARGS are extra Pi arguments."
  (interactive (progn
                 (pi-deck--enter-pane-space)
                 (list (let ((name (read-string "Agent name (empty for next number): ")))
                         (and (not (string-empty-p name)) name)))))
  (let* ((dir (file-name-as-directory (expand-file-name (or dir (pi-deck--current-space-dir)))))
         (name (or name (pi-deck--unique-session-name dir)))
         (chat (pilish-start-session dir name :args args))
         (agent (pi-deck--track chat 'pi)))
    (setf (pi-deck-agent-assigned-space agent) (pi-deck-current-space))
    (pi-deck--display-agent-in-window agent (pi-deck--new-pane-window))
    (select-window (pi-deck--input-or-chat-window agent))
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

(defun pi-deck-new-shell ()
  "Open a shell in the current space's directory in a new pane.
The first agent of a layout takes its pane."
  (interactive (progn (pi-deck--enter-pane-space) nil))
  (let* ((space (pi-deck-current-space))
         (buffer (pi-deck-start-shell (pi-deck--current-space-dir) "shell"))
         (agent (pi-deck--track buffer 'shell)))
    (setf (pi-deck-agent-assigned-space agent) space)
    (select-window (pi-deck--display-agent-in-window agent (pi-deck--new-pane-window)))
    agent))

(defun pi-deck-split-right ()
  "Split the selected pane (a chat with its input counts as one) to the right."
  (interactive)
  (select-window (pi-deck--split-pane (pi-deck--pane-window) 'right)))

(defun pi-deck-split-below ()
  "Split the selected pane (a chat with its input counts as one) below."
  (interactive)
  (select-window (pi-deck--split-pane (pi-deck--pane-window) 'below)))

(defun pi-deck-zoom ()
  "Toggle zooming the selected pane within the current layout."
  (interactive)
  (let* ((tab (tab-bar--current-tab-find))
         (saved (alist-get 'pi-deck-zoom (cdr tab)))
         (pane (pi-deck--pane-of (pi-deck--main-window))))
    (if (and saved (eq pane (window-main-window)))
        (progn (set-window-configuration saved)
               (setf (alist-get 'pi-deck-zoom (cdr tab) nil 'remove) nil))
      (pi-deck--zoom-pane pane))))

(defun pi-deck--zoom-pane (pane)
  "Make PANE the only pane of the layout; `pi-deck-zoom' restores the others."
  (setf (alist-get 'pi-deck-zoom (cdr (tab-bar--current-tab-find)))
        (current-window-configuration))
  (delete-other-windows pane))

(defun pi-deck-repair-panes ()
  "Rebuild the current layout's agent panes.
Every window showing a Pi agent's chat or input becomes that agent's chat
over its input again, including inputs that were closed."
  (interactive)
  (pi-deck--select-ordinary-window)
  (pilish-repair-window-pairs nil t)
  (pi-deck-mark-visible-seen)
  (message "pi-deck: panes repaired"))

(provide 'pi-deck-spaces)
;;; pi-deck-spaces.el ends here
