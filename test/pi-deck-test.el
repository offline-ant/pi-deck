;;; pi-deck-test.el --- pi-deck unit and integration tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Batch tests with fake Pilish sessions (chat buffers owning pipe
;; processes), real EAT terminals and the real tab bar.  No Pi processes.

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

(defmacro pi-deck-test--with-deck (&rest body)
  "Run BODY with a clean deck, tab bar and temporary state files."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "pi-deck-test-" t))
          (pi-deck-state-file (expand-file-name "state.eld" dir))
          (pi-deck-preferences-file (expand-file-name "preferences.eld" dir))
          (pi-deck--agents (make-hash-table :test 'eq))
          (pi-deck-spaces nil)
          (pi-deck--sidebar-visible nil)
          (pi-deck--pending-tabs nil)
          (pi-deck--last-tabs nil)
          (pi-deck-autosave-interval nil)
          (pi-deck-new-space-action 'dired)
          (pi-deck-test--notifications nil)
          (pi-deck-notify-function
           (lambda (title body agent) (push (list title body agent) pi-deck-test--notifications)))
          (pilish-defer-extension-dialogs nil)
          (tab-bar-tabs-function #'tab-bar-tabs)
          (kill-buffer-query-functions nil))
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
    (let* ((pilish-defer-extension-dialogs t)
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
        ;; Jumping answers the deferred dialog.
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (pi-deck-jump agent))
        (should (eq (plist-get (car responses) :confirmed) t))
        (should (eq (pi-deck-agent-state agent) 'working))
        (pi-deck-test--phase chat "idle")
        (delete-process process)
        (pi-deck-refresh)
        (should (eq (pi-deck-agent-state agent) 'unknown))))))

(ert-deftest pi-deck-test-shell-prompt-marks-and-exit ()
  (pi-deck-test--with-deck
    (let* ((buffer (pi-deck-start-shell
                    dir "eat:test"
                    '("/bin/sh" "-c" "printf '\\033]133;A\\007$ '; read a; printf '\\033]133;C\\007'; read b")))
           (agent (progn (pi-deck-refresh) (pi-deck-agent buffer))))
      (should (eq (pi-deck-agent-kind agent) 'shell))
      (should (equal (buffer-local-value 'pi-deck--shell-command buffer)
                     '("/bin/sh" "-c" "printf '\\033]133;A\\007$ '; read a; printf '\\033]133;C\\007'; read b")))
      (should (pi-deck-test--wait (lambda () (eq (pi-deck-agent-state agent) 'idle))))
      (with-current-buffer buffer (eat-term-send-string eat-terminal "go\n"))
      (should (pi-deck-test--wait (lambda () (eq (pi-deck-agent-state agent) 'working))))
      (with-current-buffer buffer (eat-term-send-string eat-terminal "end\n"))
      (should (pi-deck-test--wait (lambda () (pi-deck-refresh)
                                    (not (process-live-p (get-buffer-process buffer))))))
      (pi-deck-refresh)
      (should (eq (pi-deck-agent-state agent) 'idle))
      (should (string-match-p "exited 0" (pi-deck-agent-detail agent))))))

;;;; Spaces and tabs

(ert-deftest pi-deck-test-root-tab-is-first-and-pinned ()
  (pi-deck-test--with-deck
    (should (pi-deck-root-tab-p (car (tab-bar-tabs))))
    (pi-deck-root)
    (should (pi-deck-root-tab-p (tab-bar--current-tab-find)))
    (should (eq (window-buffer) (get-buffer pi-deck-overview-buffer-name)))
    (should-error (pi-deck-close-tab) :type 'user-error)
    (tab-bar-close-tab 1)
    (should (pi-deck-root-tab-p (car (tab-bar-tabs))))
    (pi-deck-root)
    (should-not (pi-deck-root-tab-p (tab-bar--current-tab-find)))))

(ert-deftest pi-deck-test-spaces-and-tabs ()
  (pi-deck-test--with-deck
    (let ((one (file-name-as-directory (expand-file-name "one" dir)))
          (two (file-name-as-directory (expand-file-name "two" dir))))
      (make-directory one) (make-directory two)
      (should (equal (pi-deck-create-space one) "one"))
      (should (equal (pi-deck-current-space) "one"))
      (should (equal (pi-deck-space-dir "one") one))
      (pi-deck-new-tab "second")
      (pi-deck-create-space two "two")
      (should (equal (pi-deck-space-names) (list pi-deck-root-name "one" "two")))
      ;; Tabs of a space stay together, after the root.
      (should (equal (mapcar #'pi-deck--tab-group (tab-bar-tabs))
                     (list pi-deck-root-name "one" "one" "two")))
      (pi-deck-switch-space "one")
      (should (equal (alist-get 'name (tab-bar--current-tab-find)) "second"))
      (pi-deck-next-tab)
      (should (equal (tab-bar--current-tab-index) 1))
      (pi-deck-next-tab)
      (should (equal (alist-get 'name (tab-bar--current-tab-find)) "second"))
      (pi-deck-select-tab 1)
      (should (equal (tab-bar--current-tab-index) 1))
      (pi-deck-rename-space "uno")
      (should (equal (pi-deck-current-space) "uno"))
      (should (equal (pi-deck-space-dir "uno") one))
      (pi-deck-close-space "uno")
      (should-not (pi-deck-space "uno"))
      (should (equal (mapcar #'pi-deck--tab-group (tab-bar-tabs))
                     (list pi-deck-root-name "two"))))))

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
        ;; A tab displaying an agent determines its space and status.
        (switch-to-buffer (pi-deck-agent-buffer child))
        (should (eq (pi-deck-tab-status (tab-bar--current-tab-find)) 'working))
        (pi-deck-create-space dir "elsewhere")
        (switch-to-buffer (pi-deck-agent-buffer by-dir))
        (should (equal (pi-deck-agent-space by-dir) "elsewhere"))))))

(ert-deftest pi-deck-test-new-agent-and-shell-in-space ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let (started)
      (cl-letf (((symbol-function 'pilish-start-session)
                 (lambda (directory name &rest _)
                   (push (list directory name) started)
                   (pi-deck-test--session directory name))))
        (let ((agent (pi-deck-new-agent nil)))
          (should (equal (car started) (list (file-name-as-directory dir) "1")))
          (should (eq (window-buffer) (pilish-session-input-buffer (pi-deck-agent-buffer agent))))
          (should (get-buffer-window (pi-deck-agent-buffer agent)))
          (should (equal (pi-deck-agent-space agent) "space"))
          ;; A second agent started from the input reuses the whole pane.
          (let ((second (pi-deck-new-agent "two")))
            (should (= (length (window-list nil 'no-minibuf)) 2))
            (should-not (get-buffer-window (pi-deck-agent-buffer agent)))
            (should (eq (window-buffer)
                        (pilish-session-input-buffer (pi-deck-agent-buffer second))))))))
    (let ((shell (pi-deck-new-shell t)))
      (should (eq (window-buffer) (pi-deck-agent-buffer shell)))
      (should (equal (pi-deck-agent-assigned-space shell) "space"))
      (pi-deck-zoom)
      (should (one-window-p))
      (pi-deck-zoom)
      (should-not (one-window-p)))))

(ert-deftest pi-deck-test-reveal-hidden-worker-splits-its-space ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((parent (pi-deck-agent (pi-deck-test--session dir "parent")))
           (worker (pi-deck-agent (pi-deck-test--session "/tmp/" "worker"))))
      (setf (pi-deck-agent-parent worker) (pi-deck-agent-buffer parent))
      (pi-deck-root-show)
      (pi-deck-jump worker)
      (should (equal (pi-deck-current-space) "space"))
      (should (get-buffer-window (pi-deck-agent-buffer worker)))
      ;; The small batch frame leaves no room for an input window.
      (should (eq (pilish-session-chat-buffer (window-buffer)) (pi-deck-agent-buffer worker))))))

;;;; Views

(ert-deftest pi-deck-test-overview-groups-filters-and-sorts ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((parent (pi-deck-test--session dir "parent"))
           (worker (pi-deck-test--session dir "worker"))
           (idle (pi-deck-test--session dir "quiet")))
      (switch-to-buffer parent)
      (setf (pi-deck-agent-parent (pi-deck-agent worker)) parent)
      (pi-deck-test--phase worker "thinking")
      (with-current-buffer (pi-deck-overview-buffer)
        (pi-deck-overview-refresh)
        (let ((text (buffer-string)))
          (should (string-match-p "▾ ◐ space (3)" text))
          (should (string-match-p "tab 2:" text))
          (should (string-match-p "└ worker" text))
          (should (string-match-p "not shown in a tab" text))
          (should (string-match-p "faux-model" text)))
        (pi-deck-overview-filter 'working)
        (should (string-match-p "worker" (buffer-string)))
        (should-not (string-match-p "quiet" (buffer-string)))
        (pi-deck-overview-filter nil)
        (pi-deck-overview-toggle-sort)
        (goto-char (point-min))
        (pi-deck-overview-next 1)
        (should (eq (get-text-property (point) 'pi-deck-agent) (pi-deck-agent worker)))
        ;; The worker is not shown in a tab; only its space is listed.
        (should (string-match-p "  space\\'" (buffer-substring (line-beginning-position)
                                                               (line-end-position))))
        (should (string-match-p "parent .*  space/2:" (buffer-string)))
        ;; Equal priorities list the most recent change first.
        (pi-deck-overview-next 1)
        (should (eq (get-text-property (point) 'pi-deck-agent) (pi-deck-agent idle)))))))

(ert-deftest pi-deck-test-sidebar-follows-tabs ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (pi-deck-test--session dir "a")
    (pi-deck-sidebar-toggle)
    (should (get-buffer-window pi-deck-sidebar-buffer-name))
    (should (buffer-local-value 'pi-deck-overview--compact (get-buffer pi-deck-sidebar-buffer-name)))
    (with-current-buffer pi-deck-sidebar-buffer-name
      (should (string-match-p "○ a" (buffer-string))))
    (pi-deck-new-tab nil)
    (pi-deck--on-tab-select)
    (should (get-buffer-window pi-deck-sidebar-buffer-name))
    (pi-deck-sidebar-toggle)
    (should-not (get-buffer-window pi-deck-sidebar-buffer-name))))

(ert-deftest pi-deck-test-tab-bar-cell-and-icons ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((chat (pi-deck-test--session dir "a")))
      (should-not (pi-deck-tab-bar-format-status))
      (pi-deck-test--phase chat "thinking")
      (switch-to-buffer chat)
      (should (equal (substring-no-properties (pi-deck-tab-bar-format-status)) "◐ 1 working "))
      (should (equal (substring-no-properties
                      (pi-deck-tab-name-icon "tab" (tab-bar--current-tab-find) 2))
                     "◐ tab"))
      (should (string-prefix-p "◐ " (substring-no-properties
                                     (pi-deck-tab-group-format (tab-bar--current-tab-find) 2 t))))
      (should (memq #'tab-bar-format-tabs-groups tab-bar-format))
      (pi-deck-toggle-tab-groups)
      (should (memq #'tab-bar-format-tabs tab-bar-format))
      (pi-deck-save-preferences)
      (setq pi-deck-tab-bar-groups t)
      (pi-deck-load-preferences)
      (should-not pi-deck-tab-bar-groups))))

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
          (pilish-defer-extension-dialogs t)
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

(ert-deftest pi-deck-test-layout-grid-and-workers ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((agents (mapcar (lambda (name) (pi-deck-test--session dir name)) '("a" "b" "c"))))
      (pi-deck-apply-layout "grid")
      (should (= (length (window-list nil 'no-minibuf)) 3))
      (should (cl-every (lambda (chat) (get-buffer-window chat)) agents))
      (setf (pi-deck-agent-parent (pi-deck-agent (nth 1 agents))) (nth 0 agents)
            (pi-deck-agent-parent (pi-deck-agent (nth 2 agents))) (nth 0 agents))
      (select-window (get-buffer-window (nth 0 agents)))
      (pi-deck-apply-layout "workers")
      (should (= (length (window-list nil 'no-minibuf)) 4)) ; chat, input, 2 workers
      (pi-deck-apply-layout "single")
      (should (= (length (window-list nil 'no-minibuf)) 2)))))

;;;; Persistence and daemon

(ert-deftest pi-deck-test-save-and-restore-state ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((chat (pi-deck-test--session dir "keep"))
           (shell-buffer (pi-deck-start-shell dir "eat:space" '("/bin/sh" "-c" "read line")))
           restored)
      (pi-deck-refresh)
      (switch-to-buffer chat)
      (pi-deck-new-tab "shell")
      (switch-to-buffer shell-buffer)
      (let* ((state (pi-deck-save-state))
             (records (plist-get state :agents)))
        (should (equal (mapcar (lambda (record) (plist-get record :kind)) records) '(shell pi)))
        (should (equal (plist-get (cadr records) :session-file)
                       (expand-file-name "keep.jsonl" dir)))
        (should (equal (plist-get (cadr records) :name) "keep"))
        (should (equal (plist-get (car records) :command) '("/bin/sh" "-c" "read line")))
        (should (equal (mapcar (lambda (tab) (plist-get tab :group)) (plist-get state :tabs))
                       (list pi-deck-root-name "space" "space"))))
      ;; Simulate a restart: processes, buffers and tabs are gone.
      (pi-deck-stop-agent (pi-deck-agent shell-buffer))
      (pi-deck-stop-agent (pi-deck-agent chat))
      (set-frame-parameter nil 'tabs nil)
      (setq pi-deck-spaces nil)
      (with-temp-file (expand-file-name "keep.jsonl" dir) (insert "{}\n"))
      (pi-deck-root-ensure)
      (cl-letf (((symbol-function 'pilish-start-session)
                 (lambda (directory name &rest options)
                   (push (list name (plist-get options :args)) restored)
                   (pi-deck-test--session directory name))))
        (pi-deck-restore-state))
      (should (equal restored `(("keep" ("--session" ,(expand-file-name "keep.jsonl" dir))))))
      (should (equal (pi-deck-space-dir "space") (file-name-as-directory dir)))
      (should (get-buffer "*eat:space*"))
      (should (equal (mapcar #'pi-deck--tab-group (tab-bar-tabs))
                     (list pi-deck-root-name "space" "space")))
      (should (equal (alist-get 'name (tab-bar--current-tab-find)) "shell"))
      (should (eq (window-buffer) (get-buffer "*eat:space*")))
      (pi-deck-select-tab 1)
      (should (get-buffer-window (pilish--buffer-name :chat (file-name-as-directory dir) "keep"))))))

(ert-deftest pi-deck-test-frames-receive-last-tabs ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (pi-deck-new-tab "second")
    (let ((tabs (pi-deck-frame-tabs)))
      (setq pi-deck--last-tabs tabs)
      (set-frame-parameter nil 'tabs nil)
      (cl-letf (((symbol-function 'pi-deck--client-frames) (lambda () (list (selected-frame)))))
        (pi-deck-setup-frame (selected-frame)))
      (should (equal (mapcar #'pi-deck--tab-group (tab-bar-tabs))
                     (list pi-deck-root-name "space" "space"))))))

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
        (let ((server-name "pi-test"))
          (pi-deck-restart-daemon t)))
      (should killed)
      (should (equal (seq-take helper 4) '("setsid" "-f" "sh" "-c")))
      (should (string-match-p (format "kill -0 %d" (emacs-pid)) (nth 4 helper)))
      (should (string-match-p "exec emacs --daemon.=pi-test\\'" (nth 4 helper)))
      (should (file-exists-p pi-deck-state-file))
      (should (file-exists-p (pi-deck--restore-marker)))
      (cl-letf (((symbol-function 'pi-deck-restore-state) (lambda (&rest _) 'restored)))
        (pi-deck-restore-if-pending))
      (should-not (file-exists-p (pi-deck--restore-marker))))))

(ert-deftest pi-deck-test-daemon-info-lines ()
  (pi-deck-test--with-deck
    (let ((lines (pi-deck-daemon-info-lines)))
      (should (string-match-p (format "pid %d" (emacs-pid)) (car lines)))
      (should (string-match-p "live Pi sessions 0" (car lines)))
      (should (string-match-p "orchestration" (cadr lines))))))

;;;; Orchestration

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
              (with-current-buffer (pi-deck-overview-buffer)
                (pi-deck-overview-refresh)
                (should (string-match-p "└ pi-panel:build:panel-1" (buffer-string))))))
        (pi-orchestration--close "panel-1")))))

(provide 'pi-deck-test)
;;; pi-deck-test.el ends here
