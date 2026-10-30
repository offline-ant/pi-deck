;;; pi-deck-test.el --- pi-deck unit and integration tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Batch tests with fake Pilish sessions (chat buffers owning pipe
;; processes), real ghostel terminals and real (hidden) tab-bar tabs
;; holding the spaces' layouts.  No Pi processes.

;;; Code:

(require 'ert)
(require 'pi-deck)

(defvar pi-deck-test--notifications nil
  "Notifications delivered during a test, newest first.")

(defun pi-deck-test--wait (predicate &optional seconds)
  "Process events until PREDICATE holds or SECONDS (default 5) pass."
  (let ((deadline (+ (float-time) (or seconds 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (funcall predicate)))

(defmacro pi-deck-test--with-large-frame (&rest body)
  "Run BODY in a 160x50 batch frame, room for panes side by side."
  (declare (indent 0))
  `(progn
     (set-frame-size nil 160 50)
     (unwind-protect (progn ,@body)
       (set-frame-size nil 80 25))))

(defmacro pi-deck-test--with-deck (&rest body)
  "Run BODY with a clean deck, one ungrouped tab and temporary state files."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "pi-deck-test-" t))
          (pi-deck-state-file (expand-file-name "state.eld" dir))
          (pi-deck-preferences-file (expand-file-name "preferences.eld" dir))
          (pi-deck--agents (make-hash-table :test 'eq))
          (pi-deck-spaces nil)
          (pi-deck--layouts nil)
          (pi-deck--last-space nil)
          (pi-deck-sidebar nil)
          (pi-deck-sidebar-width pi-deck-sidebar-width)
          (pi-deck-save-delay nil)
          ;; This Emacs owns the temporary state file; restores run at once.
          (pi-deck--state-claimed t)
          (pi-deck--state-owner "/tmp/pi-deck-test-owner")
          (pi-deck--state-backup-pending nil)
          (pi-deck--restoring nil)
          (pi-deck-restore-interval 0)
          (pi-deck--finished-workers nil)
          (pi-deck--pending-reveal nil)
          (pi-deck-new-space-action 'dired)
          (pi-deck-test--notifications nil)
          (pi-deck-notify-function
           (lambda (title body agent) (push (list title body agent) pi-deck-test--notifications)))
          (pilish-extension-dialog-style 'minibuffer)
          (tab-bar-tabs-function #'tab-bar-tabs)
          (kill-buffer-query-functions nil))
     (set-frame-parameter nil 'tabs nil)
     (unwind-protect
         (progn
           (pi-deck-mode 1)
           ,@body)
       (pi-deck-mode -1)
       (dolist (buffer (buffer-list))
         (when (or (pi-deck-pi-buffer-p buffer) (pi-deck-shell-buffer-p buffer)
                   (string-prefix-p "*pi-deck" (buffer-name buffer)))
           (when-let* ((process (or (get-buffer-process buffer)
                                    (and (pi-deck-pi-buffer-p buffer)
                                         (pilish-session-process buffer)))))
             (set-process-query-on-exit-flag process nil)
             (delete-process process))
           (let ((pilish-quit-without-confirmation t)) (kill-buffer buffer))))
       (set-frame-parameter nil 'tabs nil)
       (delete-other-windows)
       (switch-to-buffer "*scratch*")
       (when pi-deck--update-timer (cancel-timer pi-deck--update-timer)
             (setq pi-deck--update-timer nil))
       (delete-directory dir t))))

(defun pi-deck-test--session (dir name)
  "Return a fake Pilish chat buffer for DIR and NAME owning a pipe process."
  (let* ((dir (file-name-as-directory dir))
         (chat (generate-new-buffer (pilish--buffer-name :chat dir name)))
         (input (generate-new-buffer (pilish--buffer-name :input dir name)))
         (process (make-pipe-process :name (format "pi-deck-fake-%s" name) :noquery t
                                     :sentinel #'pilish--process-sentinel)))
    (with-current-buffer input
      (pilish-input-mode)
      (pilish--set-chat-buffer chat))
    (with-current-buffer chat
      (pilish-chat-mode)
      (pilish--set-chat-session-identity dir name)
      (pilish--set-process process)
      (pilish--set-input-buffer input)
      (setq pilish--state (list :model '(:id "faux-model")
                                :session-file (expand-file-name (format "%s.jsonl" name) dir))))
    (process-put process 'pilish-chat-buffer chat)
    (pi-deck-refresh-agent chat)
    chat))

(defun pi-deck-test--phase (chat phase)
  "Apply Pilish activity PHASE in CHAT through its public hook."
  (with-current-buffer chat (pilish--set-activity-phase phase)))

(defun pi-deck-test--main-windows ()
  "Return the selected frame's live windows outside side windows."
  (cl-remove-if (lambda (window) (window-parameter window 'window-side))
                (window-list nil 'no-minibuf)))

(defun pi-deck-test--in-dired-p (&optional window)
  "Return non-nil when WINDOW (default selected) shows Dired."
  (with-current-buffer (window-buffer window) (derived-mode-p 'dired-mode)))

;;;; Status model

(ert-deftest pi-deck-test-priority-rollup-and-summary ()
  (should (eq (pi-deck-rollup '(idle done working)) 'done))
  (should (eq (pi-deck-rollup '(idle working blocked)) 'blocked))
  (should (eq (pi-deck-rollup '(unknown idle)) 'idle))
  (should-not (pi-deck-rollup nil))
  (should (equal (mapcar #'pi-deck-status-priority pi-deck-statuses) '(4 3 2 1 0))))

(ert-deftest pi-deck-test-working-done-seen-and-notifications ()
  (pi-deck-test--with-deck
    (let* ((chat (pi-deck-test--session dir "a"))
           (agent (pi-deck-agent chat)))
      (should (eq (pi-deck-agent-status agent) 'idle))
      (pi-deck-test--phase chat "thinking")
      (should (eq (pi-deck-agent-status agent) 'working))
      (pi-deck-test--phase chat "idle")
      ;; Finished while invisible: done until shown.
      (should (eq (pi-deck-agent-status agent) 'done))
      (should (string-match-p "a finished" (car (car pi-deck-test--notifications))))
      ;; `pi-deck-notify-functions' see the same transitions, with the kind,
      ;; also when the notification itself is off.
      (let* ((events nil)
             (pi-deck-notify-functions
              (list (lambda (&rest args) (push args events))))
             (pi-deck-notify-method nil))
        (setq pi-deck-test--notifications nil)
        (pi-deck-test--phase chat "thinking")
        (pi-deck-test--phase chat "idle")
        (should-not pi-deck-test--notifications)
        (should (equal (mapcar (lambda (event) (seq-take event 2)) events)
                       '((finished "a finished"))))
        (should (eq (nth 3 (car events)) agent)))
      (switch-to-buffer chat)
      (pi-deck-mark-visible-seen)
      (should (eq (pi-deck-agent-status agent) 'idle))
      ;; Finishing while visible is seen at once and does not notify.
      (setq pi-deck-test--notifications nil)
      (pi-deck-test--phase chat "running")
      (pi-deck-test--phase chat "idle")
      (should (eq (pi-deck-agent-status agent) 'idle))
      (should-not pi-deck-test--notifications)
      ;; Sending before any event counts as work.
      (with-current-buffer chat (setq pilish--status 'sending))
      (pi-deck-refresh)
      (should (eq (pi-deck-agent-state agent) 'working)))))

(ert-deftest pi-deck-test-blocked-from-pending-dialogs ()
  (pi-deck-test--with-deck
    (let* ((pilish-extension-dialog-style 'inline)
           (chat (pi-deck-test--session dir "b"))
           (agent (pi-deck-agent chat))
           (process (pilish-session-process chat))
           responses)
      (cl-letf (((symbol-function 'pilish--send-extension-ui-response)
                 (lambda (_ response) (push response responses)))
                ((symbol-function 'message) #'ignore))
        (pi-deck-test--phase chat "running")
        (with-current-buffer chat
          (pilish--handle-extension-ui-request
           '(:type "extension_ui_request" :id "q1" :method "confirm"
             :title "Delete it?" :message "")))
        (should (eq (pi-deck-agent-status agent) 'blocked))
        (should (equal (pi-deck-agent-detail agent) "asks: Delete it?"))
        (should (string-match-p "b needs attention" (car (car pi-deck-test--notifications))))
        ;; Jumping puts point on the inline question; answering is a key press.
        (pi-deck-jump agent)
        (should (eq (window-buffer) chat))
        (should (get-text-property (point) 'pilish-dialog-choice))
        (call-interactively (key-binding (kbd "y")))
        (should (eq (plist-get (car responses) :confirmed) t))
        (should (eq (pi-deck-agent-state agent) 'working))
        (pi-deck-test--phase chat "idle")
        (delete-process process)
        (pi-deck-refresh)
        (should (eq (pi-deck-agent-state agent) 'unknown))))))

(ert-deftest pi-deck-test-shell-prompt-marks-and-exit ()
  (pi-deck-test--with-deck
    (let* ((buffer (pi-deck-start-shell
                    dir "shell:test"
                    '("/bin/sh" "-c" "printf '\\033]133;A\\007$ '; read a; printf '\\033]133;C\\007'; read b")))
           (agent (progn (pi-deck-refresh) (pi-deck-agent buffer))))
      (should (eq (pi-deck-agent-kind agent) 'shell))
      (should (equal (buffer-local-value 'pi-deck--shell-command buffer)
                     '("/bin/sh" "-c" "printf '\\033]133;A\\007$ '; read a; printf '\\033]133;C\\007'; read b")))
      (should (pi-deck-test--wait (lambda () (eq (pi-deck-agent-state agent) 'idle))))
      (with-current-buffer buffer (ghostel-send-string "go\n"))
      (should (pi-deck-test--wait (lambda () (eq (pi-deck-agent-state agent) 'working))))
      (with-current-buffer buffer (ghostel-send-string "end\n"))
      (should (pi-deck-test--wait (lambda () (pi-deck-refresh)
                                    (not (process-live-p (get-buffer-process buffer))))))
      (pi-deck-refresh)
      (should (eq (pi-deck-agent-state agent) 'idle))
      (should (string-match-p "exited 0" (pi-deck-agent-detail agent))))))

;;;; Spaces and layouts

(ert-deftest pi-deck-test-spaces-have-one-hidden-layout-each ()
  (pi-deck-test--with-deck
    (let ((one (file-name-as-directory (expand-file-name "one" dir)))
          (two (file-name-as-directory (expand-file-name "two" dir))))
      (make-directory one) (make-directory two)
      (should-not tab-bar-show)
      (should-not (pi-deck-current-space))
      ;; The first space takes the frame's ungrouped tab.
      (should (equal (pi-deck-create-space one) "one"))
      (should (= (length (tab-bar-tabs)) 1))
      (should (equal (pi-deck-current-space) "one"))
      (should (equal (pi-deck-space-dir "one") one))
      (should (pi-deck-test--in-dired-p))
      (pi-deck-create-space two "two")
      (should (equal (pi-deck-space-names) '("one" "two")))
      (should (equal (mapcar #'pi-deck--tab-group (tab-bar-tabs)) '("one" "two")))
      (should (equal pi-deck--last-space "two"))
      ;; Each space keeps its own layout; leaving one remembers it.
      (split-window-right)
      (pi-deck-switch-space "one")
      (should (one-window-p))
      (should (equal pi-deck--last-space "one"))
      (should (assoc "two" pi-deck--layouts))
      (pi-deck-switch-space "two")
      (should (= (length (window-list nil 'no-minibuf)) 2))
      (should (= (length (tab-bar-tabs)) 2))
      (pi-deck-rename-space "deux")
      (should (equal (pi-deck-current-space) "deux"))
      (should (equal (pi-deck-space-dir "deux") two))
      (should (equal pi-deck--last-space "deux"))
      (should (assoc "deux" pi-deck--layouts))
      (should-not (assoc "two" pi-deck--layouts))
      ;; Closing a space drops its layout everywhere.
      (pi-deck-close-space "deux")
      (should-not (pi-deck-space "deux"))
      (should-not (assoc "deux" pi-deck--layouts))
      (should (equal (mapcar #'pi-deck--tab-group (tab-bar-tabs)) '("one")))
      (should (equal (pi-deck-current-space) "one"))
      (should (equal pi-deck--last-space "one"))
      ;; Without spaces the frame keeps one ungrouped tab.
      (pi-deck-close-space "one")
      (should (= (length (tab-bar-tabs)) 1))
      (should-not (pi-deck-current-space))
      (should-not pi-deck--last-space))))

(ert-deftest pi-deck-test-layouts-seed-frames-without-the-space ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((a (pi-deck-test--session dir "a"))
          (b (pi-deck-test--session dir "b")))
      (pi-deck--display-agent-in-window (pi-deck-agent a) (selected-window))
      (pi-deck-show-beside (pi-deck-agent b))
      (pi-deck-sidebar-toggle)
      (pi-deck--remember-current-layout)
      ;; The sidebar is no part of a layout.
      (should-not (member pi-deck-sidebar-buffer-name
                          (window-state-buffers (cdr (assoc "space" pi-deck--layouts)))))
      ;; A new frame: one ungrouped tab with one window.
      (set-frame-parameter nil 'tabs nil)
      (pi-deck--single-window)
      (switch-to-buffer "*scratch*")
      (should-not (pi-deck-current-space))
      (pi-deck-setup-frame)
      (should (equal (pi-deck-current-space) "space"))
      (should (= (length (tab-bar-tabs)) 1))
      (should (pilish-window-pair (get-buffer-window a)))
      (should (pilish-window-pair (get-buffer-window b)))
      (should (get-buffer-window pi-deck-sidebar-buffer-name))))))

(ert-deftest pi-deck-test-new-frames-open-the-last-space ()
  (pi-deck-test--with-deck
    (let ((two (make-temp-file "pi-deck-two-" t)))
      ;; Nothing to open yet.
      (pi-deck-setup-frame)
      (should-not (pi-deck-current-space))
      (pi-deck-create-space dir "one")
      (pi-deck-create-space two "two")
      (pi-deck-switch-space "one")
      ;; A new frame shows the space selected last.
      (set-frame-parameter nil 'tabs nil)
      (pi-deck-setup-frame)
      (should (equal (pi-deck-current-space) "one"))
      (should (= (length (tab-bar-tabs)) 1))
      ;; A frame already showing a space keeps it.
      (pi-deck-switch-space "two")
      (setq pi-deck--last-space "one")
      (pi-deck-setup-frame)
      (should (equal (pi-deck-current-space) "two"))
      ;; A closing frame leaves its layout to later frames.
      (set-window-buffer (split-window-right) "*scratch*")
      (pi-deck-remember-frame (selected-frame))
      (should (member "*scratch*" (window-state-buffers (cdr (assoc "two" pi-deck--layouts)))))
      (delete-directory two t))))

(ert-deftest pi-deck-test-agent-space-resolution-and-rollup ()
  (pi-deck-test--with-deck
    (let* ((project (file-name-as-directory (expand-file-name "proj" dir)))
           (sub (expand-file-name "sub" project)))
      (make-directory sub t)
      (pi-deck-create-space project "proj")
      (let* ((by-dir (pi-deck-agent (pi-deck-test--session sub "nested")))
             (parent (pi-deck-agent (pi-deck-test--session dir "parent")))
             (child (pi-deck-agent (pi-deck-test--session dir "child"))))
        (should (equal (pi-deck-agent-space by-dir) "proj"))
        (should-not (pi-deck-agent-space parent))
        (setf (pi-deck-agent-assigned-space parent) "proj"
              (pi-deck-agent-parent child) (pi-deck-agent-buffer parent))
        (should (equal (pi-deck-agent-space child) "proj"))
        (pi-deck-test--phase (pi-deck-agent-buffer child) "thinking")
        (should (eq (cdr (assoc "proj" (pi-deck-space-statuses))) 'working))
        (pi-deck-create-space (make-temp-file "pi-deck-elsewhere-" t) "elsewhere")
        (switch-to-buffer (pi-deck-agent-buffer by-dir))
        (should (equal (pi-deck-agent-space by-dir) "proj"))
        ;; Only an agent nothing else places takes the space showing it.
        (let ((loose (pi-deck-agent (pi-deck-test--session (make-temp-file "pi-deck-loose-" t) "loose"))))
          (should-not (pi-deck-agent-space loose))
          (switch-to-buffer (pi-deck-agent-buffer loose))
          (should (equal (pi-deck-agent-space loose) "elsewhere")))))))

(ert-deftest pi-deck-test-new-agent-and-shell-in-space ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let (started)
      (cl-letf (((symbol-function 'pilish-start-session)
                 (lambda (directory name &rest _)
                   (push (list directory name) started)
                   (pi-deck-test--session directory name))))
        ;; The first agent takes the space's only pane.
        (let ((agent (pi-deck-new-agent nil)))
          (should (equal (car started) (list (file-name-as-directory dir) "1")))
          (should (eq (window-buffer) (pilish-session-input-buffer (pi-deck-agent-buffer agent))))
          (should (equal (pi-deck-agent-space agent) "space"))
          (should (= (length (window-list nil 'no-minibuf)) 2))
          ;; A second agent opens beside it in the same layout.
          (let ((second (pi-deck-new-agent "two")))
            (should (= (length (tab-bar-tabs)) 1))
            (should (= (length (window-list nil 'no-minibuf)) 4))
            (should (pilish-window-pair (get-buffer-window (pi-deck-agent-buffer agent))))
            (should (eq (window-buffer)
                        (pilish-session-input-buffer (pi-deck-agent-buffer second))))))))
    (let ((shell (pi-deck-new-shell)))
      (should (eq (window-buffer) (pi-deck-agent-buffer shell)))
      (should (equal (pi-deck-agent-assigned-space shell) "space"))
      (should (= (length (window-list nil 'no-minibuf)) 5))
      (pi-deck-zoom)
      (should (one-window-p))
      (pi-deck-zoom)
      (should-not (one-window-p))))))

(ert-deftest pi-deck-test-new-agent-outside-a-space-reads-one ()
  (pi-deck-test--with-deck
    (let ((other (make-temp-file "pi-deck-other-" t)) started)
      (pi-deck-create-space other "other-space")
      (pi-deck-create-space dir "space")
      (cl-letf (((symbol-function 'pilish-start-session)
                 (lambda (directory name &rest _)
                   (push (list directory name) started)
                   (pi-deck-test--session directory name)))
                ((symbol-function 'read-string) (lambda (&rest _) "")))
        ;; The sidebar starts agents in the space at point and shows them.
        (with-current-buffer (pi-deck-sidebar-buffer)
          (pi-deck-sidebar-refresh)
          (goto-char (point-min))
          (search-forward "other-space")
          (pi-deck-sidebar-new-agent))
        (should (equal (car started) (list (file-name-as-directory other) "1")))
        (should (equal (pi-deck-current-space) "other-space"))
        ;; A frame showing no space reads the space first.
        (set-frame-parameter nil 'tabs nil)
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "space")))
          (call-interactively #'pi-deck-new-agent))
        (should (equal (car started) (list (file-name-as-directory dir) "1")))
        (should (equal (pi-deck-current-space) "space"))
        ;; The picker lists the spaces and a "New space…" entry, which
        ;; creates a space for a picked directory.
        (set-frame-parameter nil 'tabs nil)
        (let ((fresh (make-temp-file "pi-deck-fresh-" t)) offered)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt table &rest _)
                       (if (string-prefix-p "Space:" prompt)
                           (progn (setq offered (all-completions "" table))
                                  pi-deck--new-space-choice)
                         fresh)))
                    ((symbol-function 'read-directory-name) (lambda (&rest _) fresh)))
            (call-interactively #'pi-deck-new-agent))
          (should (equal offered (list "other-space" "space" pi-deck--new-space-choice)))
          (should (equal (car started) (list (file-name-as-directory fresh) "1")))
          (should (equal (pi-deck-current-space) (file-name-nondirectory fresh)))
          (delete-directory fresh t)))
      (delete-directory other t))))

(ert-deftest pi-deck-test-space-directory-prompt-offers-typed-directory ()
  (let* ((root (directory-file-name (make-temp-file "pi-deck-root-" t)))
         (project (file-name-as-directory (expand-file-name "proj" root)))
         (projects (list project))
         (table (pi-deck--project-dir-table projects root)))
    (unwind-protect
        (progn
          (make-directory project)
          ;; An existing directory that is no project comes first, annotated.
          (should (equal (all-completions root table) (list root project)))
          (let ((metadata (completion-metadata "" table nil)))
            (should (eq (completion-metadata-get metadata 'display-sort-function) #'identity))
            (should (equal (funcall (completion-metadata-get metadata 'annotation-function) root)
                           "  (typed)"))
            (should-not (funcall (completion-metadata-get metadata 'annotation-function) project)))
          ;; Empty, missing and already listed directories add nothing.
          (should (equal (all-completions "" table) projects))
          (should (equal (all-completions (expand-file-name "nope" root) table) nil))
          (should (equal (all-completions (directory-file-name project) table) projects))
          (should (equal (all-completions project table) projects))
          ;; Relative and ~ input is checked against the base directory and home.
          (should (equal (all-completions ".." (pi-deck--project-dir-table projects project))
                         (list "..")))
          (should (equal (all-completions "~" table) (list "~")))
          ;; Styles that call the table with an empty string still see the input.
          (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t))
                    ((symbol-function 'minibuffer-contents-no-properties) (lambda () root)))
            (let* ((completion-styles '(substring))
                   (all (completion-all-completions root table nil (length root))))
              (setcdr (last all) nil)   ; drop the base size
              (should (equal (mapcar #'substring-no-properties all) (list root project)))))
          ;; The prompt returns the expanded choice; empty input takes the default.
          (let ((default-directory (file-name-as-directory root)) answer read-dir)
            (cl-letf (((symbol-function 'project-known-project-roots) (lambda () projects))
                      ((symbol-function 'completing-read)
                       (lambda (_prompt _table _pred _req _init _hist default)
                         (if (string-empty-p answer) default answer))))
              (dolist (case `(("proj" . ,(directory-file-name project))
                              ("~" . ,(expand-file-name "~"))
                              ("" . ,(file-name-as-directory root))))
                (setq answer (car case))
                (should (equal (pi-deck--read-project-dir) (cdr case)))))
            ;; Without known projects it reads a plain directory name.
            (cl-letf (((symbol-function 'project-known-project-roots) #'ignore)
                      ((symbol-function 'read-directory-name)
                       (lambda (&rest _) (setq read-dir t) "proj/")))
              (should (equal (pi-deck--read-project-dir) project))
              (should read-dir))))
      (delete-directory root t))))

;;;; Panes

(ert-deftest pi-deck-test-panes-keep-chat-and-input-together ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (cl-letf (((symbol-function 'pilish-start-session)
               (lambda (directory name &rest _) (pi-deck-test--session directory name))))
      (let* ((agent (pi-deck-new-agent "a"))
             (chat-window (get-buffer-window (pi-deck-agent-buffer agent)))
             (input-window (selected-window))
             (height (window-height input-window)))
        (should-not (eq chat-window input-window))
        (should (eq (pi-deck--pane-of input-window) (window-parent chat-window)))
        ;; Splitting splits the whole pane; the input keeps its height.
        (pi-deck-split-right)
        (let ((new (selected-window)))
          (should-not (memq new (list chat-window input-window)))
          (should-not (eq (window-parent new) (window-parent chat-window)))
          (should (= (window-height input-window) height))
          ;; Zooming the chat pane keeps chat and input; zooming again restores.
          (select-window input-window)
          (pi-deck-zoom)
          (should-not (window-live-p new))
          (should (and (window-live-p chat-window) (window-live-p input-window)))
          (pi-deck-zoom)
          (should (= (length (window-list nil 'no-minibuf)) 3)))))))

(ert-deftest pi-deck-test-hiding-panes-keeps-agents ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((a (pi-deck-test--session dir "a"))
           (b (pi-deck-test--session dir "b"))
           (shell (pi-deck-start-shell dir "shell:space" '("/bin/sh" "-c" "read line"))))
      (pi-deck-refresh)
      (pi-deck--display-agent-in-window (pi-deck-agent a) (selected-window))
      (pi-deck-show-beside (pi-deck-agent b))
      (should (pilish-window-pair (get-buffer-window a)))
      (should (pilish-window-pair (get-buffer-window b)))
      ;; A terminal takes the place of the whole pair it is shown in.
      (pi-deck--display-agent-in-window (pi-deck-agent shell) (get-buffer-window b))
      (should-not (get-buffer-window b))
      (should-not (get-buffer-window (pilish-session-input-buffer b)))
      (should (pilish-window-pair (get-buffer-window a)))
      ;; Hiding a pane hides a chat with its input; the agent runs on.
      (pi-deck-hide-pane (get-buffer-window a))
      (should-not (get-buffer-window a))
      (should-not (get-buffer-window (pilish-session-input-buffer a)))
      (should (buffer-live-p a))
      (should (pi-deck-agent a))
      ;; The only pane shows the space's directory instead.
      (select-window (get-buffer-window shell))
      (pi-deck-hide-pane)
      (should (one-window-p))
      (should (pi-deck-test--in-dired-p))
      (should (process-live-p (get-buffer-process shell)))))))

(ert-deftest pi-deck-test-repair-panes-rebuilds-pairs ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((chat (pi-deck-test--session dir "a"))
           (input (pilish-session-input-buffer chat)))
      (pi-deck--display-agent-in-window (pi-deck-agent chat) (selected-window))
      (delete-window (get-buffer-window input))
      (should-not (get-buffer-window input))
      (pi-deck-repair-panes)
      (should (eq (cdr (pilish-window-pair (get-buffer-window chat)))
                  (get-buffer-window input)))
      ;; An input shown alone gets its chat back above it.
      (pi-deck--single-window)
      (switch-to-buffer input)
      (pi-deck-repair-panes)
      (should (eq (car (pilish-window-pair (selected-window))) (get-buffer-window chat))))))

(ert-deftest pi-deck-test-reveal-focuses-switches-or-replaces ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (let ((two (make-temp-file "pi-deck-two-" t)))
      (pi-deck-create-space two "two")
      (pi-deck-create-space dir "space")
      (let* ((parent (pi-deck-agent (pi-deck-test--session dir "parent")))
             (worker (pi-deck-agent (pi-deck-test--session "/tmp/" "worker")))
             (other (pi-deck-agent (pi-deck-test--session dir "other")))
             (far (pi-deck-agent (pi-deck-test--session two "far")))
             (shown (lambda (agent) (get-buffer-window (pi-deck-agent-buffer agent)))))
        (setf (pi-deck-agent-parent worker) (pi-deck-agent-buffer parent)
              (pi-deck-agent-target worker) "t-1")
        (pi-deck--display-agent-in-window parent (selected-window))
        ;; A worker opens beside its parent on screen, focused.
        (pi-deck-reveal worker)
        (should (funcall shown parent))
        (should (eq (pilish-session-chat-buffer (window-buffer)) (pi-deck-agent-buffer worker)))
        ;; An agent on screen is just focused.
        (let ((count (length (window-list nil 'no-minibuf))))
          (pi-deck-reveal parent)
          (should (= (length (window-list nil 'no-minibuf)) count))
          (should (eq (pilish-session-chat-buffer (window-buffer)) (pi-deck-agent-buffer parent))))
        ;; Others replace the most recently used pane.
        (pi-deck-reveal other)
        (should-not (funcall shown parent))
        (should (funcall shown worker))
        (should (eq (pilish-session-chat-buffer (window-buffer)) (pi-deck-agent-buffer other)))
        ;; An agent of another space shows in that space's layout.
        (pi-deck-reveal far)
        (should (equal (pi-deck-current-space) "two"))
        (should (funcall shown far))
        (pi-deck-reveal other)
        (should (equal (pi-deck-current-space) "space"))
        (should (funcall shown worker)))
      (delete-directory two t)))))

;;;; Sidebar

(ert-deftest pi-deck-test-sidebar-follows-spaces ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (pi-deck-test--session dir "a")
    (pi-deck-sidebar-toggle)
    (should (get-buffer-window pi-deck-sidebar-buffer-name))
    (with-current-buffer pi-deck-sidebar-buffer-name
      (should (string-match-p "○ a" (buffer-string))))
    (pi-deck-create-space (make-temp-file "pi-deck-two-" t) "two")
    (should (get-buffer-window pi-deck-sidebar-buffer-name))
    (pi-deck-sidebar-toggle)
    (should-not (get-buffer-window pi-deck-sidebar-buffer-name))
    (pi-deck-switch-space "space")
    (should-not (get-buffer-window pi-deck-sidebar-buffer-name))))

(ert-deftest pi-deck-test-sidebar-width-is-shared-and-narrow-frames-hide-it ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (setq pi-deck-sidebar-width 30)
    (let ((sidebar (lambda () (get-buffer-window pi-deck-sidebar-buffer-name))))
      (pi-deck-create-space dir "space")
      (pi-deck-sidebar-toggle)
      (should (= (window-total-width (funcall sidebar)) 30))
      (pi-deck-create-space (make-temp-file "pi-deck-two-" t) "two")
      (should (= (window-total-width (funcall sidebar)) 30))
      ;; A drag: the sidebar changes width in an unchanged frame.
      (let ((window (funcall sidebar)))
        (window-resize window 6 t t)
        (cl-letf (((symbol-function 'window-old-pixel-width)
                   (lambda (window*)
                     (if (eq window* window) 1 (window-pixel-width window*)))))
          (pi-deck--on-window-size-change (selected-frame))))
      (should (= pi-deck-sidebar-width 36))
      ;; The dragged width is saved.
      (let ((pi-deck-sidebar-width 0))
        (pi-deck-load-preferences)
        (should (= pi-deck-sidebar-width 36)))
      ;; Every space's sidebar gets the new width.
      (pi-deck-switch-space "space")
      (should (= (window-total-width (funcall sidebar)) 36))
      ;; Frames without room for panes beside it hide it; it comes back.
      (set-frame-size nil (+ 36 pi-deck-sidebar-min-pane-width) 50)
      (pi-deck--sidebar-sync)
      (should (funcall sidebar))
      (set-frame-size nil (+ 35 pi-deck-sidebar-min-pane-width) 50)
      (pi-deck--sidebar-sync)
      (should-not (funcall sidebar))
      (should pi-deck-sidebar)
      (set-frame-size nil 160 50)
      ;; A frame resize resyncs after the redisplay.
      (cl-letf (((symbol-function 'window-old-pixel-width) (lambda (_) 1)))
        (pi-deck--on-window-size-change (selected-frame)))
      (should (pi-deck-test--wait (lambda () (funcall sidebar))))
      (should (= (window-total-width (funcall sidebar)) 36))))))

(ert-deftest pi-deck-test-sidebar-header-summary-and-items ()
  (pi-deck-test--with-deck
    (setq pi-deck-sidebar-width 36)
    (pi-deck-create-space dir "space")
    (pi-deck-sidebar-toggle)
    (let* ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name))
           (header (lambda () (with-selected-window sidebar (pi-deck-sidebar-header))))
           (text (lambda () (substring-no-properties (funcall header)))))
      (should (equal (buffer-local-value 'header-line-format (window-buffer sidebar))
                     '(:eval (pi-deck-sidebar-header))))
      (should (equal (funcall text) " no agents"))
      (let ((a (pi-deck-test--session dir "a"))
            (b (pi-deck-test--session dir "b")))
        (should (equal (funcall text) " all idle"))
        (pi-deck-test--phase a "thinking")
        (pi-deck-test--phase b "thinking")
        (pi-deck-test--phase b "idle")
        (should (equal (funcall text) " ● 1 done · ◐ 1 working"))
        ;; Clicking the summary goes to the next agent needing attention.
        (let ((summary (funcall header)))
          (should (eq (keymap-lookup (get-text-property 1 'local-map summary)
                                     "<header-line> <mouse-1>")
                      #'pi-deck-next-attention)))
        ;; Extra items go to the right; nil items are left out, and % is
        ;; escaped for the header line.
        (let* ((map (make-sparse-keymap))
               (pi-deck-sidebar-header-items
                (list (lambda () (propertize "mic 5%" 'local-map map)) #'ignore)))
          (should (string-match-p "working .*mic 5%%\\'" (funcall text)))
          (let ((line (funcall header)))
            (should (eq (get-text-property (string-search "mic" line) 'local-map line) map))))
        ;; Without room, status names are left out.
        (let ((pi-deck-sidebar-header-items (list (lambda () (make-string 25 ?x)))))
          (should (string-prefix-p " ● 1 · ◐ 1" (funcall text))))))))

(defun pi-deck-test--position (property value)
  "Return the first position whose PROPERTY is `equal' to VALUE, or nil."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((match (text-property-search-forward property value #'equal)))
      (prop-match-beginning match))))

(defun pi-deck-test--click (window position &optional type)
  "Return a mouse event of TYPE (default `mouse-1') at POSITION in WINDOW."
  (list (or type 'mouse-1) (list window position '(0 . 0) 0 nil
                                 position '(0 . 0) nil '(0 . 0) '(1 . 1))))

(ert-deftest pi-deck-test-sidebar-click-shows-agents ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((parent (pi-deck-test--session dir "parent"))
           (worker (pi-deck-test--session dir "worker"))
           (other (pi-deck-test--session dir "other"))
           (row-of (lambda (sidebar chat)
                     (with-current-buffer (window-buffer sidebar)
                       (pi-deck-sidebar-refresh)
                       (text-property-any (point-min) (point-max)
                                          'pi-deck-agent (pi-deck-agent chat))))))
      (setf (pi-deck-agent-parent (pi-deck-agent worker)) parent
            (pi-deck-agent-target (pi-deck-agent worker)) "t-1")
      (pi-deck--display-agent-in-window (pi-deck-agent parent) (selected-window))
      (pi-deck-sidebar-toggle)
      (let ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name)))
        (should (eq (window-parameter sidebar 'window-side) 'left))
        ;; A worker opens beside its shown parent.
        (select-window sidebar)
        (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar (funcall row-of sidebar worker)))
        (should (eq (pilish-session-chat-buffer (window-buffer)) worker))
        (should (get-buffer-window parent))
        ;; Another agent replaces the main view.
        (select-window sidebar)
        (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar (funcall row-of sidebar other)))
        (should (eq (pilish-session-chat-buffer (window-buffer)) other))
        (should-not (get-buffer-window worker))
        (should (eq (get-buffer-window pi-deck-sidebar-buffer-name) sidebar))
        ;; Middle-click shows a row beside the current pane, keeping focus.
        (select-window sidebar)
        (pi-deck-sidebar-mouse-show
         (pi-deck-test--click sidebar (funcall row-of sidebar worker) 'mouse-2))
        (should (eq (selected-window) sidebar))
        (should (get-buffer-window worker))
        (should (get-buffer-window other))
        ;; Pane commands from the sidebar act on the main view.
        (let ((count (length (pi-deck-test--main-windows))))
          (pi-deck-split-right)
          (should-not (window-parameter nil 'window-side))
          (should (> (length (pi-deck-test--main-windows)) count))
          (should (eq (get-buffer-window pi-deck-sidebar-buffer-name) sidebar))))))))

(ert-deftest pi-deck-test-sidebar-double-click-shows-only-that-agent ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((a (pi-deck-test--session dir "a"))
           (b (pi-deck-test--session dir "b")))
      (pi-deck--display-agent-in-window (pi-deck-agent a) (selected-window))
      (pi-deck-show-beside (pi-deck-agent b))
      (pi-deck-sidebar-toggle)
      (let* ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name))
             (row (with-current-buffer (window-buffer sidebar)
                    (pi-deck-sidebar-refresh)
                    (text-property-any (point-min) (point-max) 'pi-deck-agent (pi-deck-agent b)))))
        (should (eq (lookup-key pi-deck-sidebar-row-map [double-mouse-1])
                    #'pi-deck-sidebar-mouse-only))
        ;; A double click arrives as mouse-1, then double-mouse-1.
        (select-window sidebar)
        (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar row))
        (pi-deck-sidebar-mouse-only (pi-deck-test--click sidebar row 'double-mouse-1))
        (should (equal (mapcar (lambda (window) (pilish-session-chat-buffer (window-buffer window)))
                               (pi-deck-test--main-windows))
                       (list b b)))
        (should-not (get-buffer-window a))
        (should (eq (get-buffer-window pi-deck-sidebar-buffer-name) sidebar))
        ;; Zoom brings the other agent back.
        (pi-deck-zoom)
        (should (get-buffer-window a))
        (should (get-buffer-window b))
        ;; Terminals too.
        (let* ((shell (pi-deck-start-shell dir "shell:space" '("/bin/sh" "-c" "read line")))
               (shell-row (progn
                            (pi-deck-refresh)
                            (pi-deck-show-beside (pi-deck-agent shell))
                            (with-current-buffer (window-buffer sidebar)
                              (pi-deck-sidebar-refresh)
                              (text-property-any (point-min) (point-max)
                                                 'pi-deck-agent (pi-deck-agent shell))))))
          (should (get-buffer-window shell))
          (select-window sidebar)
          (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar shell-row))
          (pi-deck-sidebar-mouse-only (pi-deck-test--click sidebar shell-row 'double-mouse-1))
          (should (equal (mapcar #'window-buffer (pi-deck-test--main-windows)) (list shell)))
          (should (eq (get-buffer-window pi-deck-sidebar-buffer-name) sidebar))))))))

;; The sidebar lists agents by home space in creation order: showing,
;; revealing or moving them between spaces never reorders it.
(defun pi-deck-test--sidebar-rows (sidebar)
  "Return SIDEBAR's agent rows as (SPACE NAME MARK) in display order.
MARK is `shown' (highlighted: the frame shows it) or nil."
  (with-current-buffer (window-buffer sidebar)
    (pi-deck-sidebar-refresh)
    (let (rows space)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((agent (get-text-property (line-beginning-position) 'pi-deck-agent)))
          (when-let* ((header (get-text-property (line-beginning-position) 'pi-deck-space)))
            (setq space header))
          (when agent
            (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
                   (name (pi-deck-agent-name agent))
                   (start (string-search name line)))
              (push (list space name
                          (and (memq 'pi-deck-shown-agent
                                     (ensure-list (get-text-property start 'face line)))
                               'shown))
                    rows))))
        (forward-line 1))
      (nreverse rows))))

(ert-deftest pi-deck-test-sidebar-order-is-stable ()
  (pi-deck-test--with-deck
    (let ((two (make-temp-file "pi-deck-two-" t)))
      (pi-deck-create-space dir "space")
      (pi-deck-create-space two "two")
      (pi-deck-select-space "space")
      (let* ((zeta (pi-deck-agent (pi-deck-test--session dir "zeta")))
             (alpha (pi-deck-agent (pi-deck-test--session dir "alpha")))
             (mid (pi-deck-agent (pi-deck-test--session dir "mid")))
             (beta (pi-deck-agent (pi-deck-test--session dir "beta")))
             (order '(("space" "zeta") ("space" "mid") ("space" "alpha") ("space" "beta"))))
        (setf (pi-deck-agent-parent mid) (pi-deck-agent-buffer zeta)
              (pi-deck-agent-target mid) "t-1")
        (should (equal (pi-deck-agents) (list zeta alpha mid beta)))
        (pi-deck-sidebar-toggle)
        (let* ((sidebar (lambda () (get-buffer-window pi-deck-sidebar-buffer-name)))
               (rows (lambda () (mapcar (lambda (row) (take 2 row))
                                        (pi-deck-test--sidebar-rows (funcall sidebar)))))
               (marks (lambda () (mapcar #'cdr (pi-deck-test--sidebar-rows (funcall sidebar))))))
          ;; Creation order, the worker under its parent; not by name.
          (should (equal (funcall rows) order))
          ;; Revealing agents one after another in the same pane keeps it.
          (dolist (agent (list alpha beta zeta))
            (pi-deck-reveal agent)
            (should (equal (funcall rows) order)))
          ;; Only rows of agents the frame shows are highlighted.
          (should (equal (assoc-default "zeta" (funcall marks)) '(shown)))
          (should (equal (assoc-default "beta" (funcall marks)) '(nil)))
          ;; Shown in another space's layout, alpha stays home and in place.
          (pi-deck-select-space "two")
          (pi-deck--display-agent-in-window alpha (pi-deck--pane-window))
          (should (equal (pi-deck-agent-space alpha) "space"))
          (should (equal (funcall rows) order))
          (should (equal (assoc-default "alpha" (funcall marks)) '(shown)))
          (should (equal (assoc-default "zeta" (funcall marks)) '(nil)))
          (should-not (assoc "two" (pi-deck-space-statuses)))
          (pi-deck-select-space "space")
          (should (equal (assoc-default "alpha" (funcall marks)) '(nil)))
          ;; A new agent goes to the bottom.
          (pi-deck-test--session dir "aardvark")
          (should (equal (funcall rows) (append order '(("space" "aardvark"))))))))))

(ert-deftest pi-deck-test-agent-order-survives-restore ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (dolist (name '("b" "a" "c")) (pi-deck-test--session dir name))
    (let ((state (pi-deck-save-state)))
      (should (equal (mapcar (lambda (record) (plist-get record :name)) (plist-get state :agents))
                     '("b" "a" "c"))))
    (dolist (agent (pi-deck-agents)) (pi-deck-stop-agent agent))
    (should-not (pi-deck-agents))
    (cl-letf (((symbol-function 'pilish-start-session)
               (lambda (directory name &rest _) (pi-deck-test--session directory name))))
      (pi-deck-restore-state))
    (pi-deck-test--session dir "0")
    (should (equal (mapcar #'pi-deck-agent-name (pi-deck-agents)) '("b" "a" "c" "0")))))

(ert-deftest pi-deck-test-clicking-current-space-shows-its-directory ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (let ((two (make-temp-file "pi-deck-two-" t)))
      (pi-deck-create-space two "two")
      (pi-deck-create-space dir "space")
      (let ((chat (pi-deck-test--session dir "a")))
        (pi-deck--display-agent-in-window (pi-deck-agent chat) (pi-deck--pane-window))
        (pi-deck-sidebar-toggle)
        (let* ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name))
               (name (lambda (space)
                       (with-current-buffer (window-buffer sidebar)
                         (pi-deck-sidebar-refresh)
                         (pi-deck-test--position 'pi-deck-space space)))))
          (select-window sidebar)
          ;; Folded, the first click only unfolds.
          (with-current-buffer (window-buffer sidebar)
            (push "space" pi-deck-sidebar--collapsed))
          (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar (funcall name "space")))
          (should-not (member "space" (buffer-local-value 'pi-deck-sidebar--collapsed
                                                          (window-buffer sidebar))))
          (should (get-buffer-window chat))
          (should-not (pi-deck-test--in-dired-p))
          ;; The directory opens beside the chat and gets the focus.
          (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar (funcall name "space")))
          (should (get-buffer-window chat))
          (should (pi-deck-test--in-dired-p))
          (should (equal default-directory (file-name-as-directory dir)))
          ;; Again, it is only selected.
          (let ((count (length (pi-deck-test--main-windows)))
                (dired (selected-window)))
            (select-window (get-buffer-window chat))
            (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar (funcall name "space")))
            (should (= (length (pi-deck-test--main-windows)) count))
            (should (eq (selected-window) dired)))
          ;; Another space's name still just switches.
          (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar (funcall name "two")))
          (should (equal (pi-deck-current-space) "two"))
          (should (= (length (pi-deck-test--main-windows)) 1))))
      (delete-directory two t)))))

(ert-deftest pi-deck-test-sidebar-is-a-space-tree ()
  (pi-deck-test--with-deck
    (let ((two (make-temp-file "pi-deck-two-" t)))
      (pi-deck-create-space two "two")
      (pi-deck-create-space dir "space")
      (let* ((chat (pi-deck-test--session dir "a"))
             (panel (pi-deck-test--session dir "p"))
             popped stopped)
        (setf (pi-deck-agent-parent (pi-deck-agent panel)) chat)
        (pi-deck-sidebar-toggle)
        (let* ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name))
               (at (lambda (property value)
                     (with-current-buffer (window-buffer sidebar)
                       (pi-deck-test--position property value))))
               (button (lambda (row action)
                         (with-current-buffer (window-buffer sidebar)
                           (text-property-any row (save-excursion (goto-char row) (line-end-position)) 'pi-deck-button action)))))
          (with-current-buffer (window-buffer sidebar)
            (pi-deck-sidebar-refresh)
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "\\`two (0) ○ \\+\n" text))
              (should (string-match-p "^space (2) ○ \\+\n ○ a ✕\n   ○ p ✕\n" text))
              (should (string-match-p "\n\\+ New space\n\\'" text))))
          ;; The current space's name is highlighted.
          (should (eq (get-text-property (funcall at 'pi-deck-space "space") 'face
                                         (window-buffer sidebar))
                      'pi-deck-current-space))
          (cl-letf (((symbol-function 'pi-deck-popup) (lambda (menu &rest _) (setq popped menu)))
                    ((symbol-function 'pi-deck-stop) (lambda (agent &rest _) (setq stopped agent))))
            ;; ✕ stops its row's agent, + opens its space's menu.
            (pi-deck-sidebar-mouse-1
             (pi-deck-test--click sidebar (funcall button (funcall at 'pi-deck-agent (pi-deck-agent panel)) 'stop)))
            (should (eq stopped (pi-deck-agent panel)))
            (pi-deck-sidebar-mouse-1
             (pi-deck-test--click sidebar (funcall button (funcall at 'pi-deck-space "two") 'space-menu)))
            (should (keymapp popped))
            ;; The count folds and says so on hover; the name visits the
            ;; space and unfolds it.
            (let ((count (funcall button (funcall at 'pi-deck-space "space") 'fold)))
              (should (equal (get-text-property count 'help-echo (window-buffer sidebar))
                             "mouse-1: fold or unfold"))
              (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar count)))
            (should (member "space" (buffer-local-value 'pi-deck-sidebar--collapsed (window-buffer sidebar))))
            (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar (funcall at 'pi-deck-space "two")))
            (should (equal (pi-deck-current-space) "two"))
            (let ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name)))
              (pi-deck-sidebar-mouse-1
               (pi-deck-test--click sidebar (with-current-buffer (window-buffer sidebar)
                                              (pi-deck-test--position 'pi-deck-space "space"))))
              (should (equal (pi-deck-current-space) "space"))
              (should-not (member "space" (buffer-local-value 'pi-deck-sidebar--collapsed
                                                              (get-buffer pi-deck-sidebar-buffer-name)))))))
        (delete-directory two t)))))

(ert-deftest pi-deck-test-sidebar-rows-are-clickable ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((chat (pi-deck-test--session dir "a")) jumped popped)
      (pi-deck-sidebar-toggle)
      (let* ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name))
             (row (with-current-buffer (window-buffer sidebar)
                    (pi-deck-sidebar-refresh)
                    (text-property-any (point-min) (point-max) 'pi-deck-agent (pi-deck-agent chat)))))
        (with-current-buffer (window-buffer sidebar)
          (should (eq (get-text-property row 'mouse-face) 'highlight))
          (should (eq (keymap-lookup (get-text-property row 'keymap) "<mouse-1>")
                      #'pi-deck-sidebar-mouse-1)))
        (cl-letf (((symbol-function 'pi-deck-jump) (lambda (agent) (setq jumped agent)))
                  ((symbol-function 'pi-deck-popup) (lambda (menu &rest _) (setq popped menu))))
          (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar row))
          (should (eq jumped (pi-deck-agent chat)))
          ;; mouse-3 opens the row's menu.
          (pi-deck-sidebar-mouse-3 (pi-deck-test--click sidebar row 'mouse-3))
          (should (equal (keymap-prompt popped) "a")))))))

(ert-deftest pi-deck-test-sidebar-default-and-saved-toggle ()
  (pi-deck-test--with-deck
    (setq pi-deck-sidebar t)
    (pi-deck-create-space dir "space")
    (should (get-buffer-window pi-deck-sidebar-buffer-name))
    (pi-deck-create-space (make-temp-file "pi-deck-two-" t) "two")
    (should (get-buffer-window pi-deck-sidebar-buffer-name))
    (pi-deck-sidebar-toggle)
    (should-not (get-buffer-window pi-deck-sidebar-buffer-name))
    (pi-deck-switch-space "space")
    (should-not (get-buffer-window pi-deck-sidebar-buffer-name))
    (setq pi-deck-sidebar t)
    (pi-deck-load-preferences)
    (should-not pi-deck-sidebar)))

(ert-deftest pi-deck-test-goto-candidates-group-by-space ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((chat (pi-deck-test--session dir "a"))
          (other (pi-deck-test--session "/tmp/" "b")))
      (pi-deck-test--phase other "thinking")
      (let ((candidates (pi-deck-goto-candidates)))
        (should (= (length candidates) 2))
        (should (equal (get-text-property 0 'pi-deck-space (car (car candidates))) "space"))
        (should (eq (cdr (car candidates)) (pi-deck-agent chat)))
        (should (equal (get-text-property 0 'pi-deck-space (car (cadr candidates))) "other")))
      (should (equal (mapcar #'cdr (pi-deck-goto-candidates 'working))
                     (list (pi-deck-agent other))))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (should (eq (alist-get 'group-function (cdr (funcall table "" nil 'metadata)))
                               (alist-get 'group-function (cdr (funcall table "" nil 'metadata)))))
                   (car (car (pi-deck-goto-candidates 'working))))))
        (should (eq (pi-deck-read-agent "Agent: ") (pi-deck-agent other)))))))

(ert-deftest pi-deck-test-next-attention-prefers-blocked ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((done (pi-deck-test--session dir "done"))
          (blocked (pi-deck-test--session dir "blocked"))
          (pilish-extension-dialog-style 'inline)
          (pi-deck-answer-on-jump nil))
      (pi-deck-test--phase done "thinking")
      (pi-deck-test--phase done "idle")
      (cl-letf (((symbol-function 'message) #'ignore))
        (with-current-buffer blocked
          (pilish--handle-extension-ui-request
           '(:type "extension_ui_request" :id "q" :method "input" :title "Name"))))
      (pi-deck-next-attention)
      (should (eq (pilish-session-chat-buffer (window-buffer)) blocked))
      (pi-deck-next-attention)
      (should (eq (pilish-session-chat-buffer (window-buffer)) done))
      (should (eq (pi-deck-agent-status (pi-deck-agent done)) 'idle)))))

(ert-deftest pi-deck-test-layout-grid ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((agents (mapcar (lambda (name) (pi-deck-test--session dir name)) '("a" "b" "c"))))
      (pi-deck-layout-grid)
      ;; Each agent's cell is its chat over its input.
      (should (= (length (window-list nil 'no-minibuf)) 6))
      (should (cl-every (lambda (chat)
                          (equal (pilish-window-pair (get-buffer-window chat))
                                 (cons (get-buffer-window chat)
                                       (get-buffer-window (pilish-session-input-buffer chat)))))
                        agents)))
    (pi-deck-create-space (make-temp-file "pi-deck-empty-" t) "empty")
    (should-error (pi-deck-layout-grid) :type 'user-error)))

;;;; Stopping

(defmacro pi-deck-test--asking (answer &rest body)
  "Run BODY answering every stop question with ANSWER; return the questions."
  (declare (indent 1))
  `(let* ((questions nil)
          (pi-deck-ask-function (lambda (question choice &optional _event)
                                  (push (list question choice) questions)
                                  ,answer)))
     ,@body
     (nreverse questions)))

(defun pi-deck-test--settle ()
  "Run timers pi-deck deferred (placing panels after a target started)."
  (pi-deck-test--wait (lambda () nil) 0.05))

(ert-deftest pi-deck-test-stopping-asks-when-busy-and-removes-views-everywhere ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (let ((two (make-temp-file "pi-deck-two-" t)))
      (pi-deck-create-space two "two")
      (pi-deck-create-space dir "space")
      (let* ((a (pi-deck-test--session dir "a"))
             (worker (pi-deck-test--session dir "w"))
             (c (pi-deck-test--session dir "c")))
        (setf (pi-deck-agent-parent (pi-deck-agent worker)) a)
        ;; "space" shows A and C; "two" shows C and the worker.
        (pi-deck--display-agent-in-window (pi-deck-agent a) (selected-window))
        (pi-deck-show-beside (pi-deck-agent c))
        (pi-deck-select-space "two")
        (pi-deck--display-agent-in-window (pi-deck-agent c) (pi-deck--pane-window))
        (pi-deck-show-beside (pi-deck-agent worker))
        (pi-deck-select-space "space")
        ;; A working agent makes stopping ask, naming its workers; no keeps them.
        (pi-deck-test--phase worker "thinking")
        (should (equal (pi-deck-test--asking nil (pi-deck-stop (pi-deck-agent a)))
                       '(("Stopping a stops a (idle), w (working)." "Stop 2"))))
        (should (buffer-live-p a))
        (pi-deck-test--asking t (pi-deck-stop (pi-deck-agent a)))
        (should-not (buffer-live-p a))
        (should-not (buffer-live-p worker))
        (should (get-buffer-window c))
        (should (= (length (pi-deck-test--main-windows)) 2))
        ;; Idle agents stop without a question; the only pane shows Dired.
        (should-not (pi-deck-test--asking t (pi-deck-stop (pi-deck-agent c))))
        (should-not (buffer-live-p c))
        (should (one-window-p))
        (should (pi-deck-test--in-dired-p))
        ;; The other space's layout lost them too.
        (pi-deck-switch-space "two")
        (should (one-window-p))
        (should (pi-deck-test--in-dired-p))
        (should (= (length (tab-bar-tabs)) 2)))
      (delete-directory two t)))))

(ert-deftest pi-deck-test-closing-a-space-stops-its-agents ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((a (pi-deck-test--session dir "a"))
          (loose (pi-deck-test--session (make-temp-file "pi-deck-loose-" t) "loose")))
      (pi-deck--display-agent-in-window (pi-deck-agent a) (selected-window))
      (pi-deck-test--phase a "thinking")
      (should (equal (pi-deck-test--asking nil (pi-deck-close-space "space"))
                     '(("Closing space space stops a (working)." "Stop 1 and close"))))
      (should (pi-deck-space "space"))
      (pi-deck-test--asking t (pi-deck-close-space "space"))
      (should-not (pi-deck-space "space"))
      (should-not (buffer-live-p a))
      (should (buffer-live-p loose)))))

(ert-deftest pi-deck-test-mouse-closes-ask-with-a-dialog ()
  (let (dialog)
    (cl-letf (((symbol-function 'display-popup-menus-p) #'always)
              ((symbol-function 'x-popup-dialog)
               (lambda (_position contents &rest _) (setq dialog contents) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Keyboard question"))))
      (should (pi-deck--ask "Stopping a stops a (working)." "Stop 1"
                            (pi-deck-test--click (selected-window) 1)))
      (should (equal dialog '("Stopping a stops a (working)."
                              ("Cancel") ("Stop 1" . t)))))
    ;; From the keyboard, the minibuffer asks.
    (let ((last-nonmenu-event ?x))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) (setq dialog prompt) nil)))
        (should-not (pi-deck--ask "Q." "Stop 1" nil))
        (should (equal dialog "Q. Stop 1? "))))))

;;;; Title bars and menus

(ert-deftest pi-deck-test-pane-title-bars ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((chat (pi-deck-test--session dir "main")))
      (pi-deck--run-update)
      (with-current-buffer chat
        (should (equal (car header-line-format) pi-deck--header-construct))
        (setq pilish--cached-stats '(:contextUsage (:percent 41.4 :tokens 10)))
        (let ((header (pi-deck-pane-header)))
          (should (string-match-p "\\` ○ main · faux-model · 41%% .*\\[⤢\\] \\[✕\\] \\'"
                                  (substring-no-properties header)))
          (let ((zoom (string-search "[⤢]" header))
                (close (string-search "[✕]" header)))
            (should (eq (keymap-lookup (get-text-property zoom 'local-map header)
                                       "<header-line> <mouse-1>")
                        #'pi-deck-header-zoom))
            (should (eq (keymap-lookup (get-text-property close 'local-map header)
                                       "<header-line> <mouse-1>")
                        #'pi-deck-header-hide)))))
      ;; [⑂] forks through /fork-here without touching the draft; a
      ;; prefix function puts e.g. the window number first.
      (with-current-buffer chat
        (let* ((pi-deck-pane-header-prefix-function (lambda () "7"))
               (header (pi-deck-pane-header))
               (fork (string-search "[⑂]" header))
               submitted)
          (should (string-prefix-p "7 " (substring-no-properties header)))
          (should (eq (keymap-lookup (get-text-property fork 'local-map header)
                                     "<header-line> <mouse-1>")
                      #'pi-deck-header-fork))
          (cl-letf (((symbol-function 'pilish-submit-prompt)
                     (lambda (buffer text _callback &optional _behavior)
                       (push (list buffer text) submitted))))
            (pi-deck-fork-agent (pi-deck-agent chat))
            (should (member "Fork" (pi-deck-test--menu-labels (pi-deck-agent-menu (pi-deck-agent chat))))))
          (should (equal submitted (list (list chat "/fork-here"))))
          ;; [↑] toggles jumping to the answer for this session only.
          (let ((toggle (string-search "[↑]" header)))
            (should (eq (keymap-lookup (get-text-property toggle 'local-map header)
                                       "<header-line> <mouse-1>")
                        #'pi-deck-header-jump-to-answer))
            (should (memq 'shadow (ensure-list (get-text-property toggle 'face header))))
            (switch-to-buffer chat)
            (unwind-protect
                (progn
                  (pi-deck-header-jump-to-answer (pi-deck-test--click (selected-window) 1))
                  (should (local-variable-p 'pilish-jump-to-answer))
                  (should pilish-jump-to-answer)
                  (should-not (default-value 'pilish-jump-to-answer))
                  (let ((header (pi-deck-pane-header)))
                    (should (eq (get-text-property (string-search "[↑]" header) 'face header)
                                'header-line-highlight))))
              (kill-local-variable 'pilish-jump-to-answer)))
          ;; [↓] puts point at the end, so Pilish follows output again.
          (let ((latest (string-search "[↓]" header)))
            (should (eq (keymap-lookup (get-text-property latest 'local-map header)
                                       "<header-line> <mouse-1>")
                        #'pi-deck-header-latest))
            (switch-to-buffer chat)
            (let ((inhibit-read-only t)) (insert (make-string 50 ?\n) "end"))
            (goto-char (point-min))
            (pi-deck-header-latest (pi-deck-test--click (selected-window) 1))
            (should (= (window-point) (point-max))))
          (should (eq (keymap-lookup pi-deck-command-map "f") #'pi-deck-fork))))
      ;; A long name is truncated, never the buttons.
      (let ((long (pi-deck-test--session dir (make-string 80 ?x))))
        (switch-to-buffer long)
        (pi-deck--run-update)
        (let ((header (substring-no-properties (pi-deck-pane-header))))
          (should (string-match-p "x… *\\[↑\\] \\[↓\\] \\[⑂\\] \\[⤢\\] \\[✕\\] \\'" header))
          (should (<= (string-width (car (split-string header "\\[⤢\\]")))
                      (window-total-width)))))
      ;; Disabling the mode restores the header line.
      (pi-deck-mode -1)
      (should-not (buffer-local-value 'header-line-format chat))
      (pi-deck-mode 1))))

(defun pi-deck-test--menu-labels (menu)
  "Return the item labels of MENU, \"--\" for separators."
  (let (labels)
    (map-keymap (lambda (_key binding)
                  (when (eq (car-safe binding) 'menu-item)
                    (push (nth 1 binding) labels)))
                menu)
    (nreverse labels)))

(defun pi-deck-test--menu-command (menu label)
  "Return the command of the item LABEL in MENU."
  (let (command)
    (map-keymap (lambda (_key binding)
                  (when (and (eq (car-safe binding) 'menu-item)
                             (equal (nth 1 binding) label))
                    (setq command (nth 2 binding))))
                menu)
    command))

(ert-deftest pi-deck-test-space-menu-acts-in-its-space ()
  (pi-deck-test--with-deck
    (let ((two (make-temp-file "pi-deck-two-" t)))
      (pi-deck-create-space two "two")
      (pi-deck-create-space dir "space")
      (funcall (pi-deck-test--menu-command (pi-deck-space-menu "two") "New shell"))
      (should (equal (pi-deck-current-space) "two"))
      (should (= (length (tab-bar-tabs)) 2))
      (let ((shell (pi-deck-agent (window-buffer))))
        (should (eq (pi-deck-agent-kind shell) 'shell))
        (should (equal (pi-deck-agent-assigned-space shell) "two")))
      (delete-directory two t))))

(ert-deftest pi-deck-test-context-menus ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((chat (pi-deck-test--session dir "a")))
      (should (equal (pi-deck-test--menu-labels (pi-deck-agent-menu (pi-deck-agent chat)))
                     '("Jump" "Show beside" "Show alone" "Rename…" "--" "Fork" "Compact"
                       "Copy session file" "--" "Stop")))
      (cl-letf (((symbol-function 'message) #'ignore))
        (with-current-buffer chat
          (pilish--handle-extension-ui-request
           '(:type "extension_ui_request" :id "q" :method "input" :title "Name"))))
      (should (member "Answer question"
                      (pi-deck-test--menu-labels (pi-deck-agent-menu (pi-deck-agent chat)))))
      (should (equal (pi-deck-test--menu-labels (pi-deck-space-menu "space"))
                     '("New agent" "New shell" "--" "Rename…" "Close space")))
      (should (equal (pi-deck-test--menu-labels
                      (pi-deck-finished-worker-menu '(:name "do-1" :session-file "/tmp/x")))
                     '("Open transcript" "Resume")))
      ;; Renaming a terminal renames its buffer.
      (let ((shell (pi-deck-new-shell)))
        (pi-deck-rename-agent shell "logs")
        (should (equal (pi-deck-agent-name shell) "logs"))))))

;;;; Orchestration

(ert-deftest pi-deck-test-forks-join-their-parent-space ()
  (require 'pi-orchestration)
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((pi-orchestration--targets (make-hash-table :test 'equal))
          (parent (pi-deck-test--session dir "root")))
      (switch-to-buffer parent)
      (cl-letf (((symbol-function 'pilish-start-session)
                 (lambda (directory name &rest _) (pi-deck-test--session directory name))))
        (unwind-protect
            (progn
              (dolist (spec '(("fork-1" "interactive-fork") ("do-1" "worker")))
                (pi-orchestration--start
                 (list :id (car spec) :endpoint "test"
                       :spec (list :kind "pi" :name (car spec) :cwd "/tmp/" :placement (cadr spec)
                                   :sessionFile (expand-file-name "x.jsonl" dir)
                                   :parent (list :id (buffer-name parent))))))
              (pi-deck-test--settle)
              ;; Neither gets a layout of its own or a pane.
              (should (= (length (tab-bar-tabs)) 1))
              (should (one-window-p))
              (should (eq (window-buffer) parent))
              (let ((fork (pi-deck-agent (plist-get (pi-orchestration--target "fork-1") :buffer))))
                (should (equal (pi-deck-agent-assigned-space fork) "space"))
                (should-not (pi-deck-agent-worker-p fork)))
              (let ((worker (plist-get (pi-orchestration--target "do-1") :buffer)))
                (should (pi-deck-agent-worker-p (pi-deck-agent worker)))
                (should (equal (pi-deck-agent-space (pi-deck-agent worker)) "space"))
                ;; A finished worker does not notify.
                (setq pi-deck-test--notifications nil)
                (pi-deck-test--phase worker "thinking")
                (pi-deck-test--phase worker "idle")
                (should-not pi-deck-test--notifications)))
          (maphash (lambda (id _) (pi-orchestration--close id t)) pi-orchestration--targets))))))

(ert-deftest pi-deck-test-orchestration-workers-nest-under-parent ()
  (require 'pi-orchestration)
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((pi-orchestration--targets (make-hash-table :test 'equal))
           (parent (pi-deck-test--session dir "root")))
      (unwind-protect
          (progn
            (pi-orchestration--start
             (list :id "panel-1" :endpoint "test"
                   :spec (list :kind "shell" :name "build" :cwd "/tmp/"
                               :parent (list :id (buffer-name parent))
                               :argv ["/bin/sh" "-c" "read line"])))
            (let ((panel (pi-deck-agent (plist-get (pi-orchestration--target "panel-1") :buffer))))
              (should panel)
              (should (eq (pi-deck-agent-parent panel) parent))
              (should (equal (pi-deck-agent-target panel) "panel-1"))
              (should (equal (pi-deck-agent-space panel) "space"))
              (should-not (pi-deck--agent-record panel (pi-deck-tab-index)))
              (with-current-buffer (pi-deck-sidebar-buffer)
                (pi-deck-sidebar-refresh)
                (should (string-match-p "○ root .*\n   [^ \n]+ build "
                                        (buffer-substring-no-properties (point-min) (point-max)))))))
        (pi-orchestration--close "panel-1")))))

(ert-deftest pi-deck-test-panels-open-beside-their-parent-and-close ()
  (require 'pi-orchestration)
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((pi-orchestration--targets (make-hash-table :test 'equal))
           (parent (pi-deck-test--session dir "root"))
           (start (lambda (id)
                    (pi-orchestration--start
                     (list :id id :endpoint "test"
                           :spec (list :kind "shell" :name id :cwd "/tmp/"
                                       :parent (list :id (buffer-name parent))
                                       :argv ["/bin/sh" "-c" "read line"])))
                    (plist-get (pi-orchestration--target id) :buffer))))
      (unwind-protect
          (progn
            ;; The parent is on screen: the panel takes half of its pane, to the
            ;; right when the pane is at least twice as wide as high, unfocused.
            (switch-to-buffer parent)
            (let ((panel (funcall start "p1"))
                  (direction (pi-deck--panel-direction (selected-window))))
              (should (pi-deck-test--wait (lambda () (get-buffer-window panel))))
              (should (eq (window-buffer) parent))
              ;; The panel starts where the parent's pane ends.
              (let ((pane (window-edges (pi-deck--pane-of (get-buffer-window parent))))
                    (edges (window-edges (get-buffer-window panel)))
                    (side (if (eq direction 'right) 0 1)))
                (should (= (nth side edges) (nth (+ side 2) pane))))
              (pi-orchestration--close "p1")
              (should (= 1 (length (window-list nil 'no-minibuf)))))
            ;; The parent is not on screen: the panel is only listed, in
            ;; its parent's space, until it is revealed.
            (switch-to-buffer "*scratch*")
            (let ((panel (funcall start "p2")))
              (pi-deck-test--settle)
              (should-not (get-buffer-window panel))
              (should (= (length (tab-bar-tabs)) 1))
              (should (equal (pi-deck-agent-space (pi-deck-agent panel)) "space"))
              (pi-deck-reveal (pi-deck-agent panel))
              (should (eq (window-buffer) panel))
              ;; When Pi closes it, its pane goes.
              (pi-orchestration--close "p2")
              (should (one-window-p))
              (should (pi-deck-test--in-dired-p))))
        (maphash (lambda (id _) (pi-orchestration--close id t)) pi-orchestration--targets)))))

(defun pi-deck-test--waiting (chat text)
  "Set the worker status of CHAT to TEXT as Pi's RPC does."
  (with-current-buffer chat
    (pilish--extension-ui-set-status
     (list :statusKey "worker-frame" :statusText text))))

(ert-deftest pi-deck-test-waiting-worker-needs-attention ()
  "An interrupted worker is blocked until a new run changes its status."
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((parent (pi-deck-test--session dir "root"))
           (worker (pi-deck-test--session dir "do-1"))
           (agent (pi-deck-agent worker))
           (pilish-extension-status-functions
            (list #'pi-deck--on-extension-status)))
      (setf (pi-deck-agent-parent agent) parent
            (pi-deck-agent-target agent) "t-1")
      (should (eq (pi-deck-agent-state agent) 'idle))
      (pi-deck-test--waiting worker "worker:auto")
      (should (eq (pi-deck-agent-state agent) 'idle))
      (should-not (pi-deck-agent-detail agent))
      (let ((pi-deck-agent-change-functions (list #'pi-deck-notify-transition)))
        (pi-deck-test--waiting worker "worker:waiting · interrupted"))
      (should (eq (pi-deck-agent-status agent) 'blocked))
      (should (equal (pi-deck-agent-detail agent) "waiting for you"))
      (should (equal (car (car pi-deck-test--notifications)) "do-1 needs attention"))
      (should (equal (cadr (car pi-deck-test--notifications)) "waiting for you"))
      (should (memq agent (pi-deck--attention-agents)))
      (with-current-buffer (pi-deck-sidebar-buffer)
        (pi-deck-sidebar-refresh)
        (should (string-match-p "◉ do-1" (buffer-string))))
      ;; Other extensions' statuses are not read as waiting.
      (with-current-buffer worker
        (pilish--extension-ui-set-status
         '(:statusKey "other" :statusText "worker:waiting")))
      ;; A new run sets another status.
      (pi-deck-test--waiting worker "worker:auto")
      (should (eq (pi-deck-agent-state agent) 'idle))
      (with-current-buffer worker
        (pilish--extension-ui-set-status
         '(:statusKey "other" :statusText "worker:waiting")))
      (should (eq (pi-deck-agent-state agent) 'idle)))))

(ert-deftest pi-deck-test-mode-listens-to-extension-status ()
  (should (member '(pilish-extension-status-functions . pi-deck--on-extension-status)
                  pi-deck--hooks)))

(ert-deftest pi-deck-test-dialog-detail-beats-waiting ()
  "A question names itself even when the worker also waits."
  (pi-deck-test--with-deck
    (let* ((worker (pi-deck-test--session dir "do-1"))
           (agent (pi-deck-agent worker)))
      (pi-deck-test--waiting worker "worker:waiting")
      (cl-letf (((symbol-function 'message) #'ignore))
        (with-current-buffer worker
          (pilish--handle-extension-ui-request
           '(:type "extension_ui_request" :id "q" :method "input" :title "Name"))))
      (should (equal (pi-deck-agent-detail agent) "asks: Name")))))

(defun pi-deck-test--finished-worker (dir parent name)
  "Close a fake worker NAME of PARENT in DIR; return its finished record."
  (let* ((worker (pi-deck-test--session dir name))
         (file (expand-file-name (format "%s.jsonl" name) dir)))
    (with-temp-file file
      (insert "{\"type\":\"session\",\"version\":3,\"id\":\"s-1\",\"cwd\":\"/tmp\"}\n"))
    (setf (pi-deck-agent-parent (pi-deck-agent worker)) parent
          (pi-deck-agent-target (pi-deck-agent worker)) "t-1")
    (let ((pilish-quit-without-confirmation t))
      (delete-process (pilish-session-process worker))
      (kill-buffer (pilish-session-input-buffer worker))
      (kill-buffer worker))
    (car (pi-deck-finished-workers parent))))

(ert-deftest pi-deck-test-resume-finished-worker ()
  "Resume opens the session in a new pane of the parent's space, once."
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((parent (pi-deck-test--session dir "root"))
           (record (pi-deck-test--finished-worker dir parent "do-1"))
           (file (plist-get record :session-file))
           (other (make-temp-file "pi-deck-other-" t))
           started)
      (should (equal (plist-get record :directory) (file-name-as-directory dir)))
      (setf (pi-deck-agent-assigned-space (pi-deck-agent parent)) "space")
      (pi-deck--display-agent-in-window (pi-deck-agent parent) (selected-window))
      (pi-deck-create-space other "elsewhere")
      (should (equal (pi-deck-current-space) "elsewhere"))
      (cl-letf (((symbol-function 'pilish-start-session)
                 (lambda (directory name &rest options)
                   (push (list directory name options) started)
                   (let ((chat (pi-deck-test--session directory name)))
                     (with-current-buffer chat
                       (setq pilish--state
                             (plist-put pilish--state :session-file
                                        (plist-get options :session-file))))
                     chat))))
        (let ((agent (pi-deck-resume-finished-worker record)))
          (should (equal started (list (list (file-name-as-directory dir) "1"
                                             (list :session-file file)))))
          (should (equal (pi-deck-agent-space agent) "space"))
          (should (equal (pi-deck-current-space) "space"))
          (should-not (pi-deck-agent-worker-p agent))
          (should (pi-deck--input-or-chat-window agent))
          ;; The parent stays on screen beside it.
          (should (pi-deck--input-or-chat-window (pi-deck-agent parent)))
          ;; A second resume jumps instead of starting a second session.
          (pi-deck-select-space "elsewhere")
          (should (eq (pi-deck-resume-finished-worker record) agent))
          (should (equal (pi-deck-current-space) "space"))
          (should (= (length started) 1)))))))

(ert-deftest pi-deck-test-resume-needs-the-session-and-directory ()
  (pi-deck-test--with-deck
    (cl-letf (((symbol-function 'pilish-start-session)
               (lambda (&rest _) (ert-fail "Started a session"))))
      (should-error (pi-deck-resume-finished-worker
                     (list :name "do-1" :session-file (expand-file-name "gone.jsonl" dir)
                           :directory dir))
                    :type 'user-error)
      (let ((file (expand-file-name "there.jsonl" dir)))
        (write-region "" nil file)
        (should-error (pi-deck-resume-finished-worker
                       (list :name "do-1" :session-file file
                             :directory (expand-file-name "gone" dir)))
                      :type 'user-error)))))

(ert-deftest pi-deck-test-closed-workers-are-hidden-and-readable ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((parent (pi-deck-test--session dir "root"))
           (worker (pi-deck-test--session dir "do-1"))
           (file (expand-file-name "do-1.jsonl" dir))
           opened)
      (with-temp-file file
        (insert "{\"type\":\"session\",\"version\":3,\"id\":\"s-1\",\"cwd\":\"/tmp\"}\n"))
      (setf (pi-deck-agent-parent (pi-deck-agent worker)) parent
            (pi-deck-agent-target (pi-deck-agent worker)) "t-1")
      (let ((pilish-quit-without-confirmation t))
        (delete-process (pilish-session-process worker))
        (kill-buffer (pilish-session-input-buffer worker))
        (kill-buffer worker))
      (should-not (cl-find "do-1" (pi-deck-agents) :key #'pi-deck-agent-name :test #'equal))
      (should (equal (mapcar (lambda (record) (plist-get record :name))
                             (pi-deck-finished-workers parent))
                     '("do-1")))
      (with-current-buffer (pi-deck-sidebar-buffer)
        (pi-deck-sidebar-refresh)
        (goto-char (point-min))
        (should (search-forward "▸ 1 finished worker" nil t))
        (goto-char (point-min))
        (should (search-forward "1 finished worker" nil t))
        (should-not (string-match-p "✓ do-1" (buffer-string)))
        (pi-deck-sidebar-jump)
        (should (search-forward "✓ do-1" nil t))
        (cl-letf (((symbol-function 'pilish-open-subagent-snapshot)
                   (lambda (target) (setq opened target))))
          (pi-deck-sidebar-jump)))
      (should (equal (pilish--snapshot-target-session-id opened) "s-1"))
      (should (equal (pilish--snapshot-target-file opened) "do-1.jsonl"))
      ;; Closing the parent forgets its workers.
      (let ((pilish-quit-without-confirmation t))
        (delete-process (pilish-session-process parent))
        (kill-buffer parent))
      (should-not pi-deck--finished-workers))))

(ert-deftest pi-deck-test-clear-finished-workers ()
  (pi-deck-test--with-large-frame
   (let ((dir (make-temp-file "pi-deck-test-" t)))
     (pi-deck-test--with-deck
      (pi-deck-create-space dir "space")
      (let* ((parent (pi-deck-test--session dir "parent"))
             (other (pi-deck-test--session dir "other"))
             (record (pi-deck-test--finished-worker dir parent "do-1"))
             (other-record (pi-deck-test--finished-worker dir other "do-2")))
        (pi-deck-test--finished-worker dir parent "do-3")
        (pi-deck-sidebar-toggle)
        (let* ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name))
               (buffer (window-buffer sidebar))
               (clear (lambda (chat)
                        (with-current-buffer buffer
                          (let ((row (text-property-any (point-min) (point-max)
                                                        'pi-deck-finished chat)))
                            (next-single-property-change row 'pi-deck-button))))))
          (with-current-buffer buffer
            (pi-deck-sidebar-refresh)
            (goto-char (text-property-any (point-min) (point-max) 'pi-deck-finished parent))
            (pi-deck-sidebar-toggle-fold)
            (should (string-match-p "✓ do-1" (buffer-string))))
          ;; The ✕ clears only its parent's list, without opening anything.
          (should (eq (get-text-property (funcall clear parent) 'pi-deck-button buffer)
                      'clear-workers))
          (should (string-match-p "^ *▾ 2 finished workers +✕$"
                                  (with-current-buffer buffer (buffer-string))))
          (let ((window (selected-window)))
            (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar (funcall clear parent)))
            (should (eq (selected-window) window)))
          (should-not (pi-deck-finished-workers parent))
          (should (equal (pi-deck-finished-workers other) (list other-record)))
          (with-current-buffer buffer
            (should-not (string-match-p "do-1\\|2 finished" (buffer-string)))
            (should (string-match-p "1 finished worker" (buffer-string)))
            (should-not pi-deck-sidebar--expanded-workers))
          (should (file-readable-p (plist-get record :session-file)))
          ;; The next worker of the parent starts folded again.
          (pi-deck-test--finished-worker dir parent "do-4")
          (with-current-buffer buffer
            (pi-deck-sidebar-refresh)
            (should (string-match-p "▸ 1 finished worker" (buffer-string))))))))))

(ert-deftest pi-deck-test-finished-shells ()
  (pi-deck-test--with-large-frame
   (let ((dir (make-temp-file "pi-deck-test-" t)))
     (pi-deck-test--with-deck
      (pi-deck-create-space dir "space")
      (let* ((one (pi-deck-start-shell dir "shell:one" '("/bin/sh" "-c" "true")))
             (two (pi-deck-start-shell dir "shell:two" '("/bin/sh" "-c" "true")))
             (live (pi-deck-start-shell dir "shell:live" '("/bin/sh" "-c" "read a"))))
        (should (pi-deck-test--wait
                 (lambda () (pi-deck-refresh)
                   (not (or (get-buffer-process one) (get-buffer-process two))))))
        (pi-deck-sidebar-toggle)
        (let* ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name))
               (buffer (window-buffer sidebar))
               (text (lambda ()
                       (with-current-buffer buffer
                         (pi-deck-sidebar-refresh)
                         (buffer-substring-no-properties (point-min) (point-max))))))
          ;; Ended shells fold into one row at the end of their space; the
          ;; live one stays a row of its own.
          (should (string-match-p "^ . shell:live.*\n ▸ 2 finished shells +✕\n\n\\+ New space"
                                  (funcall text)))
          (should-not (string-match-p "exited" (funcall text)))
          (with-current-buffer buffer
            (goto-char (text-property-not-all (point-min) (point-max)
                                              'pi-deck-finished-shells nil))
            (pi-deck-sidebar-jump))
          (should (string-match-p "▾ 2 finished shells" (funcall text)))
          (should (= 2 (with-current-buffer buffer
                         (how-many "exited 0" (point-min) (point-max)))))
          ;; A shown shell keeps its row's ✕ and jump.
          (with-current-buffer buffer
            (should (text-property-any (point-min) (point-max)
                                       'pi-deck-agent (pi-deck-agent one))))
          ;; The ✕ closes the ended shells only and leaves the list folded.
          (let ((clear (with-current-buffer buffer
                         (text-property-any (point-min) (point-max)
                                            'pi-deck-button 'close-shells))))
            (pi-deck-sidebar-mouse-1 (pi-deck-test--click sidebar clear)))
          (should-not (or (buffer-live-p one) (buffer-live-p two)))
          (should (buffer-live-p live))
          (should-not (string-match-p "finished shell" (funcall text)))
          (should-not (buffer-local-value 'pi-deck-sidebar--expanded-shells buffer))))))))

;;;; Persistence and daemon

(ert-deftest pi-deck-test-save-and-restore-state ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (let ((two (make-temp-file "pi-deck-two-" t)))
      (pi-deck-create-space two "two")
      (pi-deck-create-space dir "space")
      (let* ((chat (pi-deck-test--session dir "keep"))
             (fresh (pi-deck-test--session dir "fresh"))
             (shell-buffer (pi-deck-start-shell dir "shell:space" '("/bin/sh" "-c" "read line")))
             (chat-name (buffer-name chat))
             restored)
        (pi-deck-refresh)
        (pi-deck--display-agent-in-window (pi-deck-agent chat) (selected-window))
        (pi-deck-show-beside (pi-deck-agent shell-buffer))
        (let* ((state (pi-deck-save-state))
               (records (plist-get state :agents)))
          (should (eq (plist-get state :version) 2))
          (should (equal (plist-get state :space) "space"))
          (should (equal (sort (mapcar #'car (plist-get state :layouts))) '("space" "two")))
          (should (member chat-name (window-state-buffers
                                     (cdr (assoc "space" (plist-get state :layouts))))))
          (should (equal (sort (mapcar (lambda (record) (plist-get record :kind)) records))
                         '(pi pi shell)))
          (let ((keep (seq-find (lambda (record) (equal (plist-get record :name) "keep")) records)))
            (should (equal (plist-get keep :session-file) (expand-file-name "keep.jsonl" dir))))
          (should (equal (plist-get (seq-find (lambda (record) (eq (plist-get record :kind) 'shell))
                                              records)
                                    :command)
                         '("/bin/sh" "-c" "read line"))))
        ;; Simulate a restart: processes, buffers, tabs and spaces are gone.
        (pi-deck-stop-agent (pi-deck-agent shell-buffer))
        (pi-deck-stop-agent (pi-deck-agent chat))
        (pi-deck-stop-agent (pi-deck-agent fresh))
        (set-frame-parameter nil 'tabs nil)
        (pi-deck--single-window)
        (switch-to-buffer "*scratch*")
        (setq pi-deck-spaces nil pi-deck--layouts nil pi-deck--last-space nil)
        (with-temp-file (expand-file-name "keep.jsonl" dir) (insert "{}\n"))
        (cl-letf (((symbol-function 'pilish-start-session)
                   (lambda (directory name &rest options)
                     (push (list name (plist-get options :session-file)) restored)
                     (pi-deck-test--session directory name))))
          (pi-deck-restore-state))
        ;; A session without a file yet (no history) restarts fresh.
        (should (equal (sort restored :key #'car)
                       `(("fresh" nil) ("keep" ,(expand-file-name "keep.jsonl" dir)))))
        (should (equal (pi-deck-space-names) '("two" "space")))
        (should (equal (pi-deck-space-dir "space") (file-name-as-directory dir)))
        ;; The frame shows the saved space with its layout, paired again.
        (should (equal (pi-deck-current-space) "space"))
        (should (= (length (tab-bar-tabs)) 1))
        (should (pilish-window-pair (get-buffer-window chat-name)))
        (should (get-buffer-window "*shell:space*"))
        (pi-deck-switch-space "two")
        (should (= (length (tab-bar-tabs)) 2)))
      (delete-directory two t)))))

(ert-deftest pi-deck-test-restart-daemon-saves-and-detaches ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (pi-deck-test--session dir "live")
    (let (helper killed)
      (cl-letf (((symbol-function 'pi-deck-server-running-p) #'always)
                ((symbol-function 'call-process)
                 (lambda (program _infile destination _display &rest args)
                   (setq helper (cons program args))
                   (should (eq destination 0))))
                ((symbol-function 'kill-emacs) (lambda (&rest _) (setq killed t))))
        (let ((server-name "pi-test")
              (server-socket-dir "/tmp/pi-deck-test-sockets"))
          (pi-deck-restart-daemon t)))
      (should killed)
      (should (equal (seq-take helper 4) '("setsid" "-f" "sh" "-c")))
      (should (string-match-p (format "kill -0 %d" (emacs-pid)) (nth 4 helper)))
      ;; The replacement binds the running socket file, not a re-resolved name.
      (should (string-match-p "exec emacs --daemon.=/tmp/pi-deck-test-sockets/pi-test\\'"
                              (nth 4 helper)))
      (should (file-exists-p pi-deck-state-file))
      (should (equal (plist-get (pi-deck-read-state) :owner) "/tmp/pi-deck-test-owner")))))

(defun pi-deck-test--write-state (file owner names dir)
  "Write a state to FILE owned by OWNER with Pi sessions NAMES in DIR."
  (with-temp-file file
    (prin1 (list :version 2 :owner owner
                 :spaces (list (list :name "space" :dir (file-name-as-directory dir)))
                 :space "space" :layouts nil
                 :agents (mapcar (lambda (name)
                                   (list :kind 'pi :buffer (format "*restored-%s*" name)
                                         :dir dir :name name :session-file nil :space "space"))
                                 names))
           (current-buffer))))

(defmacro pi-deck-test--claiming (socket started &rest body)
  "Run BODY with a running server on SOCKET, recording session names in STARTED."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'pi-deck-server-socket) (lambda () ,socket))
             ((symbol-function 'pilish-start-session)
              (lambda (directory name &rest _)
                (push name ,started)
                (pi-deck-test--session directory name))))
     (let ((pi-deck--state-claimed nil)
           (pi-deck--state-owner nil))
       ,@body)))

(ert-deftest pi-deck-test-owner-restores-on-start ()
  (pi-deck-test--with-deck
    (let (started)
      (pi-deck-test--write-state pi-deck-state-file "/s/pi" '("a" "b") dir)
      (pi-deck-test--claiming "/s/pi" started
        (pi-deck-claim-state)
        (should (equal pi-deck--state-owner "/s/pi"))
        (should (equal (reverse started) '("a" "b")))
        (should (pi-deck-space "space"))
        ;; Claiming happens once per Emacs.
        (pi-deck-claim-state)
        (should (= (length started) 2))))))

(ert-deftest pi-deck-test-other-emacs-neither-restores-nor-saves ()
  (pi-deck-test--with-deck
    (let ((pi-deck-save-delay 0.01) started)
      (pi-deck-test--write-state pi-deck-state-file "/s/pi" '("a") dir)
      (let ((before (with-temp-buffer (insert-file-contents pi-deck-state-file) (buffer-string))))
        ;; Another daemon, then an Emacs without a server.
        (dolist (socket '("/s/other" nil))
          (pi-deck-test--claiming socket started
            (pi-deck-claim-state)
            (should-not pi-deck--state-owner)
            (pi-deck-create-space dir (format "own-%s" (if socket "other" "none")))
            (pi-deck--save-now)
            (pi-deck-test--wait (lambda () nil) 0.1)))
        (should-not started)
        (should (equal (with-temp-buffer (insert-file-contents pi-deck-state-file) (buffer-string))
                       before))))))

(ert-deftest pi-deck-test-explicit-save-takes-over-the-state ()
  (pi-deck-test--with-deck
    (let (started)
      (dolist (owner '(nil "/s/pi"))
        (pi-deck-test--write-state pi-deck-state-file owner '("a") dir)
        (setq pi-deck--last-saved nil)
        (pi-deck-test--claiming "/s/other" started
          (pi-deck-claim-state)
          (should-not pi-deck--state-owner)
          (pi-deck-save-state)
          (should (equal pi-deck--state-owner "/s/other"))
          (should (equal (plist-get (pi-deck-read-state) :owner) "/s/other"))))
      (should-not started))))

(ert-deftest pi-deck-test-unrestored-state-is-backed-up ()
  (pi-deck-test--with-deck
    (let ((pi-deck-auto-restore nil) started)
      (pi-deck-test--write-state pi-deck-state-file "/s/pi" '("a") dir)
      (pi-deck-test--claiming "/s/pi" started
        (pi-deck-claim-state)
        (should (equal pi-deck--state-owner "/s/pi"))
        (should-not started)
        (pi-deck-create-space dir "new")
        (pi-deck--save-now)
        (let ((backup (concat pi-deck-state-file ".bak")))
          (should (equal (mapcar (lambda (r) (plist-get r :name))
                                 (plist-get (pi-deck-read-state backup) :agents))
                         '("a")))
          (should (pi-deck-space "new"))
          (should-not (plist-get (pi-deck-read-state) :agents))
          ;; Only the first save moves the old file away.
          (delete-file backup)
          (pi-deck-create-space dir "newer")
          (pi-deck-save-state)
          (should-not (file-exists-p backup)))))))

(ert-deftest pi-deck-test-restore-is-staggered ()
  (pi-deck-test--with-deck
    (let ((pi-deck-restore-interval 0.05) started)
      (pi-deck-test--write-state pi-deck-state-file "/s/pi" '("a" "b" "c") dir)
      (pi-deck-test--claiming "/s/pi" started
        (pi-deck-claim-state)
        (should (equal started '("a")))
        (should (timerp pi-deck--restoring))
        ;; No save while sessions are still starting: the file lists them all.
        (pi-deck--save-now)
        (should (= 3 (length (plist-get (pi-deck-read-state) :agents))))
        (should-error (pi-deck-restore-state) :type 'user-error)
        (should (pi-deck-test--wait (lambda () (null pi-deck--restoring))))
        (should (equal (reverse started) '("a" "b" "c")))
        (pi-deck--save-now)
        (should (= 3 (length (plist-get (pi-deck-read-state) :agents))))))))

(ert-deftest pi-deck-test-daemon-claims-after-server-start ()
  (pi-deck-test--with-deck
    (pi-deck-mode -1)
    (let ((pi-deck--state-claimed nil)
          (emacs-startup-hook nil))
      (cl-letf (((symbol-function 'daemonp) #'always)
                ((symbol-function 'pi-deck-server-running-p) #'ignore))
        (pi-deck-mode 1))
      (should-not pi-deck--state-claimed)
      (should (memq #'pi-deck-claim-state emacs-startup-hook))
      (pi-deck-mode -1)
      (should-not (memq #'pi-deck-claim-state emacs-startup-hook)))))

(ert-deftest pi-deck-test-server-socket-absolute-name ()
  (cl-letf (((symbol-function 'pi-deck-server-running-p) #'always))
    (let ((server-name "/tmp/emacs1/pi")
          (server-socket-dir "/elsewhere"))
      (should (equal (pi-deck-server-socket) "/tmp/emacs1/pi"))
      (should (equal (pi-deck-server-display-name) "pi"))
      (should (equal (pi-deck-restart-command-line) '("emacs" "--daemon=/tmp/emacs1/pi"))))))

(ert-deftest pi-deck-test-daemon-info ()
  (pi-deck-test--with-deck
    (let ((lines (pi-deck-daemon-info-lines)))
      (should (string-match-p (format "pid %d" (emacs-pid)) (car lines)))
      (should (string-match-p "live Pi sessions 0" (car lines)))
      (should (string-match-p "orchestration" (cadr lines)))
      (pi-deck-daemon-info)
      (with-current-buffer "*pi-deck daemon*"
        (should (equal (string-trim-right (buffer-string)) (string-join lines "\n")))))))

(ert-deftest pi-deck-test-show-session-uses-or-creates-its-space ()
  (pi-deck-test--with-deck
    (should (eq pi-orchestration-show-session-function #'pi-deck-show-session))
    (let* ((other (file-name-as-directory (make-temp-file "pi-deck-switch-" t)))
           (other-space (file-name-nondirectory (directory-file-name other))))
      (cl-letf (((symbol-function 'pi-deck--client-frames) (lambda () (list (selected-frame))))
                ((symbol-function 'select-frame-set-input-focus) #'ignore))
        ;; A directory without a space gets one, showing the pair.
        (let ((agent (pi-deck-show-session (pi-deck-test--session other "switched"))))
          (should (equal (pi-deck-agent-assigned-space agent) other-space))
          (should (equal (pi-deck-current-space) other-space))
          (should (eq (window-buffer) (pilish-session-input-buffer (pi-deck-agent-buffer agent))))
          (should (get-buffer-window (pi-deck-agent-buffer agent))))
        ;; An existing space for the directory shows it, from elsewhere too.
        (pi-deck-create-space dir "space")
        (pi-deck-select-space other-space)
        (let ((agent (pi-deck-show-session (pi-deck-test--session dir "second"))))
          (should (equal (pi-deck-agent-assigned-space agent) "space"))
          (should (equal (pi-deck-current-space) "space"))
          (should (eq (window-buffer) (pilish-session-input-buffer (pi-deck-agent-buffer agent))))
          ;; Showing it again focuses it without adding panes.
          (let ((count (length (window-list nil 'no-minibuf))))
            (pi-deck-select-space other-space)
            (pi-deck-show-session (pi-deck-agent-buffer agent))
            (should (equal (pi-deck-current-space) "space"))
            (should (= (length (window-list nil 'no-minibuf)) count)))
          (should (= (length (tab-bar-tabs)) 2))))
      ;; Without a client frame, the next frame shows it.
      (cl-letf (((symbol-function 'pi-deck--client-frames) (lambda () nil)))
        (let ((agent (pi-deck-show-session (pi-deck-test--session other "later"))))
          (should (eq pi-deck--pending-reveal (pi-deck-agent-buffer agent)))))
      (cl-letf (((symbol-function 'pi-deck--client-frames) (lambda () (list (selected-frame)))))
        (pi-deck-setup-frame (selected-frame)))
      (should-not pi-deck--pending-reveal)
      (should (equal (pi-deck-current-space) other-space))
      (should (string-match-p "later" (buffer-name (window-buffer))))
      (delete-directory other t))))

(ert-deftest pi-deck-test-state-saves-after-changes ()
  (pi-deck-test--with-deck
    (let ((pi-deck-save-delay 0.05))
      (pi-deck-create-space dir "space")
      (should (pi-deck-test--wait (lambda () (file-exists-p pi-deck-state-file))))
      (should-not pi-deck--save-timer)
      (should (= (logand (file-modes pi-deck-state-file) #o077) 0))
      (delete-file pi-deck-state-file)
      ;; An agent finishing a turn saves again; nothing saves periodically.
      (let ((chat (pi-deck-test--session dir "a")))
        (pi-deck-test--wait (lambda () (file-exists-p pi-deck-state-file)))
        (delete-file pi-deck-state-file)
        (pi-deck-test--wait (lambda () nil) 0.2)
        (should-not (file-exists-p pi-deck-state-file))
        (pi-deck-test--phase chat "thinking")
        (pi-deck-test--phase chat "idle")
        (should (pi-deck-test--wait (lambda () (file-exists-p pi-deck-state-file))))
        (should (cl-find "a" (plist-get (pi-deck-read-state) :agents)
                         :key (lambda (record) (plist-get record :name)) :test #'equal))))))

(provide 'pi-deck-test)
;;; pi-deck-test.el ends here
