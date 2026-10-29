;;; pi-deck-test.el --- pi-deck unit and integration tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Batch tests with fake Pilish sessions (chat buffers owning pipe
;; processes), real ghostel terminals and the real tab bar.  No Pi processes.

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
  "Run BODY with a clean deck, tab bar and temporary state files."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "pi-deck-test-" t))
          (pi-deck-state-file (expand-file-name "state.eld" dir))
          (pi-deck-preferences-file (expand-file-name "preferences.eld" dir))
          (pi-deck--agents (make-hash-table :test 'eq))
          (pi-deck-spaces nil)
          (pi-deck-sidebar nil)
          (pi-deck--pending-tabs nil)
          (pi-deck--last-tabs nil)
          (pi-deck-save-delay nil)
          (pi-deck--finished-workers nil)
          (pi-deck--pending-reveal nil)
          (pi-deck-new-space-action 'dired)
          (pi-deck-test--notifications nil)
          (pi-deck-notify-function
           (lambda (title body agent) (push (list title body agent) pi-deck-test--notifications)))
          (pilish-extension-dialog-style 'minibuffer)
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
        ;; A tab displaying an agent determines the tab's status, not the
        ;; agent's home space.
        (switch-to-buffer (pi-deck-agent-buffer child))
        (should (eq (pi-deck-tab-status (tab-bar--current-tab-find)) 'working))
        (pi-deck-create-space (make-temp-file "pi-deck-elsewhere-" t) "elsewhere")
        (switch-to-buffer (pi-deck-agent-buffer by-dir))
        (should (equal (pi-deck-agent-space by-dir) "proj"))
        ;; Only an agent nothing else places takes a displaying tab's space.
        (let ((loose (pi-deck-agent (pi-deck-test--session (make-temp-file "pi-deck-loose-" t) "loose"))))
          (should-not (pi-deck-agent-space loose))
          (switch-to-buffer (pi-deck-agent-buffer loose))
          (should (equal (pi-deck-agent-space loose) "elsewhere")))))))

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
          ;; A second agent gets a tab of its own in the space.
          (let ((second (pi-deck-new-agent "two")))
            (should (= (length (pi-deck--space-tab-entries "space")) 2))
            (should (= (length (window-list nil 'no-minibuf)) 2))
            (should-not (get-buffer-window (pi-deck-agent-buffer agent)))
            (should (equal (alist-get 'name (tab-bar--current-tab-find)) "two"))
            (should (eq (window-buffer)
                        (pilish-session-input-buffer (pi-deck-agent-buffer second))))))))
    (let ((shell (pi-deck-new-shell t)))
      (should (eq (window-buffer) (pi-deck-agent-buffer shell)))
      (should (equal (pi-deck-agent-assigned-space shell) "space"))
      (pi-deck-zoom)
      (should (one-window-p))
      (pi-deck-zoom)
      (should-not (one-window-p)))))

(ert-deftest pi-deck-test-new-agent-from-root-uses-a-space ()
  (pi-deck-test--with-deck
    (let ((other (make-temp-file "pi-deck-other-" t)) started)
      (pi-deck-create-space other "other-space")
      (pi-deck-create-space dir "space")
      (cl-letf (((symbol-function 'pilish-start-session)
                 (lambda (directory name &rest _)
                   (push (list directory name) started)
                   (pi-deck-test--session directory name)))
                ((symbol-function 'read-string) (lambda (&rest _) "")))
        ;; The overview starts agents in the space at point and shows them.
        (pi-deck-root-show)
        (with-current-buffer (pi-deck-overview-buffer)
          (pi-deck-overview-refresh)
          (goto-char (point-min))
          (search-forward "other-space")
          (pi-deck-overview-new-agent))
        (should (equal (car started) (list (file-name-as-directory other) "1")))
        (should (equal (pi-deck-current-space) "other-space"))
        ;; Elsewhere on the root tab, the space is read first.
        (pi-deck-root-show)
        (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "space")))
          (call-interactively #'pi-deck-new-agent))
        (should (equal (car started) (list (file-name-as-directory dir) "1")))
        (should (equal (pi-deck-current-space) "space"))
        ;; The picker lists the spaces and a "New space…" entry, which
        ;; creates a space for a picked directory.
        (pi-deck-root-show)
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

(ert-deftest pi-deck-test-two-agents-side-by-side-stay-paired ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (cl-letf (((symbol-function 'pilish-start-session)
               (lambda (directory name &rest _) (pi-deck-test--session directory name))))
      (let* ((a (pi-deck-new-agent "a"))
             (b (pi-deck-new-agent "b"))
             (chat-a (pi-deck-agent-buffer a))
             (chat-b (pi-deck-agent-buffer b)))
        ;; B has its own tab; shown beside A in A's tab, both stay pairs.
        (pi-deck-select-tab 1)
        (pi-deck-show-beside b)
        (should (pilish-window-pair (get-buffer-window chat-a)))
        (should (pilish-window-pair (get-buffer-window chat-b)))
        (should-not (eq (window-parent (get-buffer-window chat-a))
                        (window-parent (get-buffer-window chat-b))))
        ;; A shell takes the place of the whole pair it is opened in.
        (select-window (get-buffer-window (pilish-session-input-buffer chat-b)))
        (pi-deck-new-shell)
        (should-not (get-buffer-window chat-b))
        (should-not (get-buffer-window (pilish-session-input-buffer chat-b)))
        (should (pilish-window-pair (get-buffer-window chat-a)))
        ;; Closing a pane closes the chat with its input and stops A, which
        ;; nothing else shows; B lives on in its own tab.
        (let ((input-a (pilish-session-input-buffer chat-a)))
          (select-window (get-buffer-window chat-a))
          (pi-deck-close-pane)
          (should-not (buffer-live-p chat-a))
          (should-not (get-buffer-window input-a))
          (should (buffer-live-p chat-b))))))))

(ert-deftest pi-deck-test-repair-tab-rebuilds-pairs ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((chat (pi-deck-test--session dir "a"))
           (input (pilish-session-input-buffer chat)))
      (pi-deck--display-agent-in-window (pi-deck-agent chat) (selected-window))
      (delete-window (get-buffer-window input))
      (should-not (get-buffer-window input))
      (pi-deck-repair-tab)
      (should (eq (cdr (pilish-window-pair (get-buffer-window chat)))
                  (get-buffer-window input)))
      ;; An input shown alone gets its chat back above it.
      (pi-deck--single-window)
      (switch-to-buffer input)
      (pi-deck-repair-tab)
      (should (eq (car (pilish-window-pair (selected-window))) (get-buffer-window chat))))))

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
          (should (string-match-p "tab parent" text))
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
        (should (string-match-p "parent .*  space/parent" (buffer-string)))
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

(defun pi-deck-test--main-windows ()
  "Return the selected frame's live windows outside side windows."
  (cl-remove-if (lambda (window) (window-parameter window 'window-side))
                (window-list nil 'no-minibuf)))

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
                       (pi-deck-overview-refresh)
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
        (pi-deck-overview-mouse-1 (pi-deck-test--click sidebar (funcall row-of sidebar worker)))
        (should (eq (pilish-session-chat-buffer (window-buffer)) worker))
        (should (get-buffer-window parent))
        ;; Another agent replaces the main view.
        (select-window sidebar)
        (pi-deck-overview-mouse-1 (pi-deck-test--click sidebar (funcall row-of sidebar other)))
        (should (eq (pilish-session-chat-buffer (window-buffer)) other))
        (should-not (get-buffer-window worker))
        (should (eq (get-buffer-window pi-deck-sidebar-buffer-name) sidebar))
        ;; Middle-click shows a row beside the current pane, keeping focus.
        (select-window sidebar)
        (pi-deck-overview-mouse-show
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

;; The sidebar lists agents by home space in creation order: showing,
;; revealing or moving them between tabs never reorders it.
(defun pi-deck-test--sidebar-rows (sidebar)
  "Return SIDEBAR's agent rows as (SPACE NAME MARK) in display order.
MARK is `bold' (shown in this tab), `elsewhere' (◦) or nil."
  (with-current-buffer (window-buffer sidebar)
    (pi-deck-overview-refresh)
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
                          (cond ((eq (get-text-property start 'face line) 'bold) 'bold)
                                ((string-match-p (concat (regexp-quote name) " ◦") line) 'elsewhere)))
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
        (let ((sidebar (lambda () (get-buffer-window pi-deck-sidebar-buffer-name)))
              (rows (lambda (sidebar) (mapcar (lambda (row) (take 2 row))
                                              (pi-deck-test--sidebar-rows sidebar)))))
          ;; Creation order, the worker under its parent; not by name.
          (should (equal (funcall rows (funcall sidebar)) order))
          ;; Revealing agents one after another in the same pane keeps it.
          (dolist (agent (list alpha beta zeta))
            (pi-deck-reveal agent 'window)
            (should (equal (funcall rows (funcall sidebar)) order)))
          ;; zeta in the current tab is bold; shown in another tab, ◦.
          (should (equal (assoc-default "zeta" (mapcar #'cdr (pi-deck-test--sidebar-rows (funcall sidebar))))
                         '(bold)))
          (pi-deck-new-tab "other")
          (pi-deck--display-agent-in-window beta (pi-deck--pane-window))
          (pi-deck-select-tab 1)
          (let ((marks (mapcar #'cdr (pi-deck-test--sidebar-rows (funcall sidebar)))))
            (should (equal (assoc-default "zeta" marks) '(bold)))
            (should (equal (assoc-default "beta" marks) '(elsewhere)))
            (should (equal (assoc-default "alpha" marks) '(nil))))
          ;; Shown in another space's tab, alpha stays home and in place.
          (pi-deck-select-space "two")
          (pi-deck--display-agent-in-window alpha (pi-deck--pane-window))
          (should (equal (pi-deck-agent-space alpha) "space"))
          (should (equal (funcall rows (funcall sidebar)) order))
          (should-not (assoc "two" (pi-deck-space-statuses)))
          ;; A new agent goes to the bottom.
          (pi-deck-test--session dir "aardvark")
          (should (equal (funcall rows (funcall sidebar))
                         (append order '(("space" "aardvark"))))))))))

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
            (pi-deck-overview-refresh)
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "\\` ⌂ Overview\n" text))
              (should (string-match-p "▾ ○ two (0) \\+\n" text))
              (should (string-match-p "▾ ○ space (2) \\+\n   ○ a ✕\n     ○ p ✕\n" text))
              (should (string-match-p "\n \\+ New space\n\\'" text))
              (should-not (string-match-p pi-deck-root-name text))))
          ;; The current space's name is highlighted.
          (should (eq (get-text-property (+ 5 (funcall at 'pi-deck-space "space")) 'face
                                         (window-buffer sidebar))
                      'pi-deck-current-space))
          (cl-letf (((symbol-function 'pi-deck-popup) (lambda (menu &rest _) (setq popped menu)))
                    ((symbol-function 'pi-deck-stop) (lambda (agent &rest _) (setq stopped agent))))
            ;; ✕ stops its row's agent, + opens its space's menu.
            (pi-deck-overview-mouse-1
             (pi-deck-test--click sidebar (funcall button (funcall at 'pi-deck-agent (pi-deck-agent panel)) 'stop)))
            (should (eq stopped (pi-deck-agent panel)))
            (pi-deck-overview-mouse-1
             (pi-deck-test--click sidebar (funcall button (funcall at 'pi-deck-space "two") 'space-menu)))
            (should (keymapp popped))
            ;; The arrow folds; the name visits the space and unfolds it.
            (pi-deck-overview-mouse-1
             (pi-deck-test--click sidebar (funcall button (funcall at 'pi-deck-space "space") 'fold)))
            (should (member "space" (buffer-local-value 'pi-deck-overview--collapsed (window-buffer sidebar))))
            (pi-deck-overview-mouse-1 (pi-deck-test--click sidebar (funcall at 'pi-deck-space "two")))
            (should (equal (pi-deck-current-space) "two"))
            (let ((sidebar (get-buffer-window pi-deck-sidebar-buffer-name)))
              (pi-deck-overview-mouse-1
               (pi-deck-test--click sidebar (with-current-buffer (window-buffer sidebar)
                                              (pi-deck-test--position 'pi-deck-space "space"))))
              (should (equal (pi-deck-current-space) "space"))
              (should-not (member "space" (buffer-local-value 'pi-deck-overview--collapsed
                                                              (get-buffer pi-deck-sidebar-buffer-name))))
              (setq sidebar (get-buffer-window pi-deck-sidebar-buffer-name))
              ;; ⌂ shows the overview.
              (pi-deck-overview-mouse-1 (pi-deck-test--click sidebar 2))
              (should (pi-deck-root-tab-p (tab-bar--current-tab-find))))))
        (delete-directory two t)))))

(defun pi-deck-test--tab-bar-items ()
  "Return the tab bar items as (KEY LABEL BINDING), without separators."
  (cl-loop for item in (pi-deck-tab-bar-format-space)
           unless (string-match-p "sep" (symbol-name (car item)))
           collect (list (car item) (substring-no-properties (nth 2 item)) (nth 3 item))))

;; Long names shrink with "…" so every tab keeps its ✕ in the frame.
(ert-deftest pi-deck-test-tab-bar-names-fit ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((long (make-string 60 ?n))
           (chat (pi-deck-test--session dir long)))
      (switch-to-buffer chat)
      (pi-deck-new-tab nil)
      (let ((pi-deck-tab-name-max-width 30))
        (dolist (width '(160 60))
          (set-frame-size nil width 30)
          (let* ((labels (mapcar (lambda (item) (nth 1 item))
                                 (cl-remove-if-not (lambda (item) (string-match-p "\\`\\(current-tab\\|tab-[0-9]+\\)\\'" (symbol-name (car item))))
                                                   (pi-deck-test--tab-bar-items))))
                 (total (apply #'+ (mapcar (lambda (item) (string-width (nth 2 item)))
                                           (tab-bar-format-list tab-bar-format)))))
            (should (= (length labels) 2))
            (should (cl-some (lambda (label) (string-match-p "n… ✕ \\'" label)) labels))
            (dolist (label labels)
              (should (string-match-p "✕ \\'" label))
              (should (<= (string-width label) 36)))
            (should (<= total width))))))))

(ert-deftest pi-deck-test-tab-bar-shows-the-current-space ()
  (pi-deck-test--with-deck
    (let ((two (make-temp-file "pi-deck-two-" t)))
      (pi-deck-create-space two "two")
      (pi-deck-create-space dir "space")
      (let ((chat (pi-deck-test--session dir "a")))
        (should-not (pi-deck-tab-bar-format-status))
        (pi-deck-test--phase chat "thinking")
        (switch-to-buffer chat)
        (pi-deck-new-tab nil)
        (let ((items (pi-deck-test--tab-bar-items)))
          ;; ⌂, the space button, this space's two tabs (named after their
          ;; agent, with icons and ✕), then +.  The root and "two" are absent.
          (should (equal (mapcar #'car items) '(pi-deck-home pi-deck-space tab-3 current-tab pi-deck-add)))
          (should (equal (nth 1 (nth 1 items)) " ◐ space ▾ "))
          (should (string-match-p "\\` ◐ a ✕ *\\'" (nth 1 (nth 2 items))))
          (should (eq (nth 2 (nth 0 items)) 'pi-deck-root))
          (should (eq (nth 2 (nth 1 items)) 'pi-deck-spaces-menu))
          (should (eq (nth 2 (nth 4 items)) 'pi-deck-new-tab-menu))
          ;; Non-current tabs select through tab-bar's default (nil binding).
          (should-not (nth 2 (nth 2 items))))
        (let ((label (nth 2 (nth 2 (cl-remove-if (lambda (item) (string-match-p "sep" (symbol-name (car item))))
                                                 (pi-deck-tab-bar-format-space))))))
          (should (get-text-property (string-search "✕" label) 'close-tab label)))
        ;; Numbers count within the space.
        (pi-deck-toggle-tab-numbers)
        (should (string-match-p " 2 " (nth 1 (nth 3 (pi-deck-test--tab-bar-items)))))
        (pi-deck-save-preferences)
        (setq pi-deck-tab-numbers nil)
        (pi-deck-load-preferences)
        (should pi-deck-tab-numbers)
        ;; From the overview, the most recent space's tabs remain.
        (pi-deck-root-show)
        (should (equal (mapcar #'car (pi-deck-test--tab-bar-items))
                       '(pi-deck-home pi-deck-space tab-3 tab-4 pi-deck-add)))
        (should (equal (substring-no-properties (pi-deck-tab-bar-status-string)) "◐ 1 working "))
        (should (eq (nth 3 (car (pi-deck-tab-bar-format-status))) 'pi-deck-tab-bar-status-click)))
      (delete-directory two t))))

(ert-deftest pi-deck-test-tab-bar-mouse-and-stable-names ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((chat (pi-deck-test--session dir "main"))
          (shell (pi-deck-test--session dir "zz-panel"))
          item closed popped)
      (pi-deck--display-agent-in-window (pi-deck-agent chat) (selected-window))
      (should (equal (pi-deck-tab-name) "main"))
      ;; Selecting another pane does not rename the tab.
      (let ((window (split-window (pi-deck--pane-of (selected-window)) nil 'right)))
        (set-window-buffer window shell)
        (select-window window)
        (should (equal (pi-deck-tab-name) "main")))
      (should (eq (keymap-lookup tab-bar-map "<mouse-1>") #'pi-deck-tab-bar-mouse-1))
      (cl-letf (((symbol-function 'pi-deck--tab-bar-item) (lambda (_event) item))
                ((symbol-function 'pi-deck-close-tab) (lambda (&optional n) (setq closed (or n 'current))))
                ((symbol-function 'pi-deck-popup) (lambda (menu &rest _) (setq popped menu))))
        ;; ✕ and mouse-2 close through pi-deck's close.
        (setq item '(tab-3 nil t))
        (pi-deck-tab-bar-mouse-1 nil)
        (should (eq closed 3))
        (setq item '(current-tab ignore nil) closed nil)
        (pi-deck-tab-bar-mouse-2 nil)
        (should (eq closed 'current))
        ;; mouse-3 opens the tab's or the space's menu.
        (setq item '(current-tab ignore nil))
        (pi-deck-tab-bar-mouse-3 nil)
        (should (equal (keymap-prompt popped) (format "Tab %s" (alist-get 'name (tab-bar--current-tab-find)))))
        (setq item '(pi-deck-space pi-deck-spaces-menu nil))
        (pi-deck-tab-bar-mouse-3 nil)
        (should (equal (keymap-prompt popped) "Space space"))))
    (pi-deck-mode -1)
    (should-not (eq (keymap-lookup tab-bar-map "<mouse-1>") #'pi-deck-tab-bar-mouse-1))
    (pi-deck-mode 1)))

(ert-deftest pi-deck-test-tab-bar-extra-items ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((clicks nil)
           (command (lambda (event) (interactive "e") (push (event-basic-type event) clicks)))
           (pi-deck-tab-bar-extra-items
            (list (lambda () `((test-mic menu-item " mic " ,command :help "test"))))))
      (should (eq (car (last (pi-deck--tab-bar-format))) #'pi-deck-tab-bar-format-extra))
      (should (equal (mapcar #'car (pi-deck-tab-bar-format-extra)) '(test-mic)))
      (let ((item (list 'test-mic command nil)) (tabs (pi-deck--tabs)))
        (cl-letf (((symbol-function 'pi-deck--tab-bar-item) (lambda (_event) item))
                  ((symbol-function 'pi-deck-popup) (lambda (&rest _) (error "No menu expected"))))
          ;; Pressing does nothing; releases run the command with the click.
          (pi-deck-tab-bar-mouse-down-1 '(down-mouse-1 nil))
          (should (equal (pi-deck--tabs) tabs))
          (pi-deck-tab-bar-mouse-1 '(mouse-1 nil))
          (pi-deck-tab-bar-mouse-3 '(mouse-3 nil))
          (should (equal clicks '(mouse-3 mouse-1))))))))

(ert-deftest pi-deck-test-tab-bar-text-terminal-hit-test ()
  ;; Labels are placed by display width and right-aligned items at the edge.
  (let* ((width (frame-inner-width))
         (tab-bar-format
          (list (lambda () `((wide menu-item ,(concat "🎤" (propertize "✕" 'close-tab t)) left)))
                #'tab-bar-format-align-right
                (lambda () '((right menu-item "ab" right-command))))))
    (should (equal (pi-deck--tab-bar-item-at 0) '(wide left nil)))
    (should (equal (pi-deck--tab-bar-item-at 1) '(wide left nil)))
    (should (equal (pi-deck--tab-bar-item-at 2) '(wide left t)))
    (should (eq (car (pi-deck--tab-bar-item-at 5)) 'align-right))
    (should (equal (pi-deck--tab-bar-item-at (- width 2)) '(right right-command nil)))
    (should (equal (pi-deck--tab-bar-item-at (- width 1)) '(right right-command nil)))
    (should-not (pi-deck--tab-bar-item-at width))))

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

(ert-deftest pi-deck-test-layout-grid-and-workers ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((agents (mapcar (lambda (name) (pi-deck-test--session dir name)) '("a" "b" "c"))))
      (pi-deck-apply-layout "grid")
      ;; Each agent's cell is its chat over its input.
      (should (= (length (window-list nil 'no-minibuf)) 6))
      (should (cl-every (lambda (chat)
                          (equal (pilish-window-pair (get-buffer-window chat))
                                 (cons (get-buffer-window chat)
                                       (get-buffer-window (pilish-session-input-buffer chat)))))
                        agents))
      (setf (pi-deck-agent-parent (pi-deck-agent (nth 1 agents))) (nth 0 agents)
            (pi-deck-agent-parent (pi-deck-agent (nth 2 agents))) (nth 0 agents))
      (select-window (get-buffer-window (nth 0 agents)))
      (pi-deck-apply-layout "workers")
      (should (= (length (window-list nil 'no-minibuf)) 6)) ; three chat and input pairs
      (pi-deck-apply-layout "single")
      (should (= (length (window-list nil 'no-minibuf)) 2)))))

;;;; Closing

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
  "Run timers pi-deck deferred (stopping after a tab closed)."
  (pi-deck-test--wait (lambda () nil) 0.05))

(ert-deftest pi-deck-test-closing-a-tab-stops-what-only-it-shows ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((a (pi-deck-test--session dir "a"))
           (b (pi-deck-test--session dir "b"))
           (worker (pi-deck-test--session dir "w"))
           (shown (pi-deck-test--session dir "s")))
      (setf (pi-deck-agent-parent (pi-deck-agent worker)) a
            (pi-deck-agent-target (pi-deck-agent worker)) "t-w"
            (pi-deck-agent-parent (pi-deck-agent shown)) a)
      ;; Tab 1 shows A and B; tab 2 shows B and S (A's child).
      (pi-deck--display-agent-in-window (pi-deck-agent a) (selected-window))
      (pi-deck-show-beside (pi-deck-agent b))
      (pi-deck-new-tab nil)
      (pi-deck--display-agent-in-window (pi-deck-agent b) (selected-window))
      (pi-deck-show-beside (pi-deck-agent shown))
      (pi-deck-select-tab 1)
      (let ((tab (tab-bar--current-tab-find)))
        ;; A and its viewless worker stop; B and S have other views.
        (should (equal (pi-deck-tab-close-plan tab)
                       (list (pi-deck-agent a) (pi-deck-agent worker))))
        ;; Idle agents stop without a question.
        (pi-deck-test--phase worker "thinking")
        ;; A working one asks, naming everything that stops; no closes it.
        (let ((questions (pi-deck-test--asking nil (pi-deck-close-tab))))
          (should (equal questions
                         '(("Closing tab a stops a (idle), w (working)." "Stop 2 and close")))))
        (should (= (length (pi-deck--space-tab-entries "space")) 2))
        (should (buffer-live-p a))
        ;; Yes closes the tab and stops them.
        (pi-deck-test--asking t (pi-deck-close-tab))
        (pi-deck-test--settle)
        (should (= (length (pi-deck--space-tab-entries "space")) 1))
        (should-not (buffer-live-p a))
        (should-not (buffer-live-p worker))
        (should (buffer-live-p b))
        (should (buffer-live-p shown))
        (should (equal (pi-deck-current-space) "space")))
      ;; The tab bar's ✕ goes through the same rule: tab-bar's own close
      ;; runs `pi-deck--tab-close-guard'.
      (pi-deck-test--phase b "thinking")
      (let ((questions (pi-deck-test--asking nil
                         (tab-bar-close-tab (1+ (tab-bar--current-tab-index))))))
        (should (equal (cadr (car questions)) "Stop 2 and close")))
      (should (buffer-live-p b))
      ;; The overview tab is pinned.
      (tab-bar-close-tab 1)
      (should (pi-deck-root-tab-p (car (tab-bar-tabs))))))))

(ert-deftest pi-deck-test-closing-panes-agents-and-spaces ()
  (pi-deck-test--with-large-frame
   (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((a (pi-deck-test--session dir "a"))
           (b (pi-deck-test--session dir "b"))
           (c (pi-deck-test--session dir "c")))
      (pi-deck--display-agent-in-window (pi-deck-agent a) (selected-window))
      (pi-deck-show-beside (pi-deck-agent b))
      (pi-deck-show-beside (pi-deck-agent c))
      ;; C is shown twice: closing one of its panes stops nothing.
      (let ((second (pi-deck--split-pane (get-buffer-window c) 'right)))
        (pi-deck--display-agent-in-window (pi-deck-agent c) second)
        (should-not (pi-deck-pane-close-plan second))
        (pi-deck-close-pane second)
        (should (buffer-live-p c))
        (should (get-buffer-window c)))
      ;; B's only pane: a working B asks; yes stops it.
      (pi-deck-test--phase b "thinking")
      (should (equal (pi-deck-test--asking t (pi-deck-close-pane (get-buffer-window b)))
                     '(("Closing this pane stops b (working)." "Stop 1 and close"))))
      (should-not (buffer-live-p b))
      ;; Stopping C from its row removes its pane too.
      (pi-deck-stop (pi-deck-agent c))
      (should-not (buffer-live-p c))
      (should (get-buffer-window a))
      ;; The tab's only pane closes the tab.
      (pi-deck-new-tab nil)
      (should (= (length (pi-deck--space-tab-entries "space")) 2))
      (let ((d (pi-deck-test--session dir "d")))
        (pi-deck--display-agent-in-window (pi-deck-agent d) (selected-window))
        (pi-deck-close-pane)
        (pi-deck-test--settle)
        (should-not (buffer-live-p d)))
      (should (= (length (pi-deck--space-tab-entries "space")) 1))
      ;; Closing the space stops its agents; working ones make it ask.
      (pi-deck-test--phase a "thinking")
      (should (equal (cadr (car (pi-deck-test--asking nil (pi-deck-close-space "space"))))
                     "Stop 1 and close"))
      (should (pi-deck-space "space"))
      (pi-deck-test--asking t (pi-deck-close-space "space"))
      (should-not (pi-deck-space "space"))
      (should-not (buffer-live-p a))))))

(ert-deftest pi-deck-test-mouse-closes-ask-with-a-dialog ()
  (let (dialog)
    (cl-letf (((symbol-function 'display-popup-menus-p) #'always)
              ((symbol-function 'x-popup-dialog)
               (lambda (_position contents &rest _) (setq dialog contents) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Keyboard question"))))
      (should (pi-deck--ask "Closing tab a stops a (working)." "Stop 1 and close"
                            (pi-deck-test--click (selected-window) 1)))
      (should (equal dialog '("Closing tab a stops a (working)."
                              ("Cancel") ("Stop 1 and close" . t)))))
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
                        #'pi-deck-header-close)))))
      ;; A long name is truncated, never the buttons.
      (let ((long (pi-deck-test--session dir (make-string 80 ?x))))
        (switch-to-buffer long)
        (pi-deck--run-update)
        (let ((header (substring-no-properties (pi-deck-pane-header))))
          (should (string-match-p "x… *\\[⤢\\] \\[✕\\] \\'" header))
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

(ert-deftest pi-deck-test-context-menus ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((chat (pi-deck-test--session dir "a")))
      (should (equal (pi-deck-test--menu-labels (pi-deck-agent-menu (pi-deck-agent chat)))
                     '("Jump" "Show beside" "Zoom" "Rename…" "--" "Stop")))
      (cl-letf (((symbol-function 'message) #'ignore))
        (with-current-buffer chat
          (pilish--handle-extension-ui-request
           '(:type "extension_ui_request" :id "q" :method "input" :title "Name"))))
      (should (member "Answer question"
                      (pi-deck-test--menu-labels (pi-deck-agent-menu (pi-deck-agent chat)))))
      (should (equal (pi-deck-test--menu-labels (pi-deck-space-menu "space"))
                     '("New agent" "New shell" "New tab" "--" "Rename…" "Close space")))
      (should (equal (pi-deck-test--menu-labels (pi-deck-tab-context-menu 2))
                     '("Rename…" "Close")))
      (should (equal (pi-deck-test--menu-labels (pi-deck--new-tab-menu-map))
                     '("New agent" "New shell" "Empty tab")))
      (should (equal (pi-deck-test--menu-labels
                      (pi-deck-finished-worker-menu '(:name "do-1" :session-file "/tmp/x")))
                     '("Open transcript")))
      ;; Renaming a terminal renames its buffer.
      (let ((shell (pi-deck-new-shell)))
        (pi-deck-rename-agent shell "logs")
        (should (equal (pi-deck-agent-name shell) "logs"))))))

(ert-deftest pi-deck-test-forks-get-tabs-workers-do-not ()
  (require 'pi-orchestration)
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((pi-orchestration--targets (make-hash-table :test 'equal))
          (parent (pi-deck-test--session dir "root")))
      (switch-to-buffer parent)
      (cl-letf (((symbol-function 'pilish-start-session)
                 (lambda (directory name &rest _) (pi-deck-test--session directory name))))
        (unwind-protect
            (let ((tabs (length (tab-bar-tabs))))
              (dolist (spec '(("fork-1" "interactive-fork") ("do-1" "worker")))
                (pi-orchestration--start
                 (list :id (car spec) :endpoint "test"
                       :spec (list :kind "pi" :name (car spec) :cwd dir :placement (cadr spec)
                                   :sessionFile (expand-file-name "x.jsonl" dir)
                                   :parent (list :id (buffer-name parent))))))
              (should (pi-deck-test--wait (lambda () (= (length (tab-bar-tabs)) (1+ tabs)))))
              (pi-deck-test--settle)
              (should (= (length (tab-bar-tabs)) (1+ tabs)))
              (should (eq (window-buffer) parent))
              (let* ((fork (plist-get (pi-orchestration--target "fork-1") :buffer))
                     (tab (car (last (tab-bar-tabs)))))
                (should (equal (pi-deck--tab-group tab) "space"))
                (should (memq fork (pi-deck--tab-buffers tab (selected-frame))))
                (should-not (pi-deck-agent-worker-p (pi-deck-agent fork))))
              (should (pi-deck-agent-worker-p
                       (pi-deck-agent (plist-get (pi-orchestration--target "do-1") :buffer)))))
          (maphash (lambda (id _) (pi-orchestration--close id t)) pi-orchestration--targets))))))

;;;; Persistence and daemon

(ert-deftest pi-deck-test-save-and-restore-state ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let* ((chat (pi-deck-test--session dir "keep"))
           (fresh (pi-deck-test--session dir "fresh"))
           (shell-buffer (pi-deck-start-shell dir "shell:space" '("/bin/sh" "-c" "read line")))
           restored)
      (pi-deck-refresh)
      (switch-to-buffer chat)
      (pi-deck-new-tab "shell")
      (switch-to-buffer shell-buffer)
      ;; Tabs outside spaces are not saved.
      (let ((tab-bar-new-tab-group nil)) (tab-bar-new-tab))
      (switch-to-buffer "*scratch*")
      (tab-bar-switch-to-prev-tab)
      (let* ((state (pi-deck-save-state))
             (records (plist-get state :agents)))
        (should (equal (sort (mapcar (lambda (record) (plist-get record :kind)) records))
                       '(pi pi shell)))
        (let ((keep (seq-find (lambda (record) (equal (plist-get record :name) "keep")) records)))
          (should (equal (plist-get keep :session-file) (expand-file-name "keep.jsonl" dir))))
        (should (equal (plist-get (seq-find (lambda (record) (eq (plist-get record :kind) 'shell))
                                            records)
                                  :command)
                       '("/bin/sh" "-c" "read line")))
        (should (equal (mapcar (lambda (tab) (plist-get tab :group)) (plist-get state :tabs))
                       (list pi-deck-root-name "space" "space"))))
      ;; Simulate a restart: processes, buffers and tabs are gone.
      (pi-deck-stop-agent (pi-deck-agent shell-buffer))
      (pi-deck-stop-agent (pi-deck-agent chat))
      (pi-deck-stop-agent (pi-deck-agent fresh))
      (set-frame-parameter nil 'tabs nil)
      (setq pi-deck-spaces nil)
      (with-temp-file (expand-file-name "keep.jsonl" dir) (insert "{}\n"))
      (pi-deck-root-ensure)
      (cl-letf (((symbol-function 'pilish-start-session)
                 (lambda (directory name &rest options)
                   (push (list name (plist-get options :session-file)) restored)
                   (pi-deck-test--session directory name))))
        (pi-deck-restore-state))
      ;; A session without a file yet (no history) restarts fresh.
      (should (equal (sort restored :key #'car)
                     `(("fresh" nil) ("keep" ,(expand-file-name "keep.jsonl" dir)))))
      (should (equal (pi-deck-space-dir "space") (file-name-as-directory dir)))
      (should (get-buffer "*shell:space*"))
      (should (equal (mapcar #'pi-deck--tab-group (tab-bar-tabs))
                     (list pi-deck-root-name "space" "space")))
      (should (equal (alist-get 'name (tab-bar--current-tab-find)) "shell"))
      (should (eq (window-buffer) (get-buffer "*shell:space*")))
      (pi-deck-select-tab 1)
      (should (get-buffer-window (pilish--buffer-name :chat (file-name-as-directory dir) "keep"))))))

(ert-deftest pi-deck-test-save-without-frames-keeps-last-tabs ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((tabs (plist-get (pi-deck-save-state) :tabs)))
      (should (equal (mapcar (lambda (tab) (plist-get tab :group)) tabs)
                     (list pi-deck-root-name "space")))
      ;; All client terminals died without `delete-frame-functions'.
      (cl-letf (((symbol-function 'pi-deck--client-frames) #'ignore))
        (should (equal (plist-get (pi-deck-save-state) :tabs) tabs))))))

(ert-deftest pi-deck-test-frames-receive-last-tabs ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (pi-deck-new-tab "second")
    (let ((tabs (pi-deck-frame-tabs)))
      (setq pi-deck--last-tabs tabs)
      (set-frame-parameter nil 'tabs nil)
      ;; The new frame's single tab shows the sidebar too; it is still replaced.
      (delete-other-windows)
      (pi-deck-sidebar-toggle)
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
      (should (file-exists-p (pi-deck--restore-marker)))
      (cl-letf (((symbol-function 'pi-deck-restore-state) (lambda (&rest _) 'restored)))
        (pi-deck-restore-if-pending))
      (should-not (file-exists-p (pi-deck--restore-marker))))))

(ert-deftest pi-deck-test-server-socket-absolute-name ()
  (cl-letf (((symbol-function 'pi-deck-server-running-p) #'always))
    (let ((server-name "/tmp/emacs1/pi")
          (server-socket-dir "/elsewhere"))
      (should (equal (pi-deck-server-socket) "/tmp/emacs1/pi"))
      (should (equal (pi-deck-server-display-name) "pi"))
      (should (equal (pi-deck-restart-command-line) '("emacs" "--daemon=/tmp/emacs1/pi"))))))

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
                (should (string-match-p "└ build " (buffer-string))))))
        (pi-orchestration--close "panel-1")))))

(ert-deftest pi-deck-test-panels-show-in-parent-space-and-close ()
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
            ;; The parent is on screen: the panel opens below it, unfocused.
            (switch-to-buffer parent)
            (let ((panel (funcall start "p1")))
              (should (pi-deck-test--wait (lambda () (get-buffer-window panel))))
              (should (eq (window-buffer) parent))
              (pi-orchestration--close "p1")
              (should (= 1 (length (window-list nil 'no-minibuf)))))
            ;; The parent is not on screen: a background tab of its space.
            (switch-to-buffer "*scratch*")
            (let* ((count (length (tab-bar-tabs)))
                   (current (tab-bar--current-tab-index))
                   (panel (funcall start "p2")))
              (should (pi-deck-test--wait (lambda () (= (length (tab-bar-tabs)) (1+ count)))))
              (should (= (tab-bar--current-tab-index) current))
              (let ((tab (nth (1+ current) (tab-bar-tabs))))
                (should (equal (pi-deck--tab-group tab) "space"))
                (should (memq panel (pi-deck--tab-buffers tab (selected-frame)))))
              (pi-orchestration--close "p2")
              (should (= (length (tab-bar-tabs)) count))))
        (maphash (lambda (id _) (pi-orchestration--close id t)) pi-orchestration--targets)))))

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
      (with-current-buffer (pi-deck-overview-buffer)
        (pi-deck-overview-refresh)
        (goto-char (point-min))
        (should (search-forward "1 finished worker" nil t))
        (should-not (string-match-p "✓ do-1" (buffer-string)))
        (pi-deck-overview-jump)
        (should (search-forward "✓ do-1" nil t))
        (cl-letf (((symbol-function 'pilish-open-subagent-snapshot)
                   (lambda (target) (setq opened target))))
          (pi-deck-overview-jump)))
      (should (equal (pilish--snapshot-target-session-id opened) "s-1"))
      (should (equal (pilish--snapshot-target-file opened) "do-1.jsonl"))
      ;; The sidebar folds them into the same row.
      (with-current-buffer (pi-deck-overview-buffer t)
        (pi-deck-overview-refresh)
        (should (string-match-p "▸ 1 finished worker" (buffer-string)))
        (should-not (string-match-p "do-1" (buffer-string))))
      ;; Closing the parent forgets its workers.
      (let ((pilish-quit-without-confirmation t))
        (delete-process (pilish-session-process parent))
        (kill-buffer parent))
      (should-not pi-deck--finished-workers))))

(ert-deftest pi-deck-test-overview-rows-are-clickable ()
  (pi-deck-test--with-deck
    (pi-deck-create-space dir "space")
    (let ((chat (pi-deck-test--session dir "a")) jumped popped)
      (pi-deck-root-show)
      (with-current-buffer (pi-deck-overview-buffer)
        (pi-deck-overview-refresh)
        (let* ((row (text-property-any (point-min) (point-max) 'pi-deck-agent (pi-deck-agent chat)))
               (header (next-single-property-change (point-min) 'pi-deck-space))
               (fold (text-property-any header (point-max) 'pi-deck-button 'fold)))
          (should (eq (get-text-property row 'mouse-face) 'highlight))
          (should (eq (keymap-lookup (get-text-property row 'keymap) "<mouse-1>")
                      #'pi-deck-overview-mouse-1))
          (cl-letf (((symbol-function 'pi-deck-jump) (lambda (agent) (setq jumped agent)))
                    ((symbol-function 'pi-deck-popup) (lambda (menu &rest _) (setq popped menu))))
            (pi-deck-overview-mouse-1 (pi-deck-test--click (selected-window) row))
            (should (eq jumped (pi-deck-agent chat)))
            ;; mouse-3 opens the row's menu.
            (pi-deck-overview-mouse-3 (pi-deck-test--click (selected-window) row 'mouse-3))
            (should (equal (keymap-prompt popped) "a")))
          ;; The arrow folds the space.
          (pi-deck-overview-mouse-1 (pi-deck-test--click (selected-window) fold))
          (should (member "space" pi-deck-overview--collapsed))))
      ;; The header's name visits the space.
      (with-current-buffer (pi-deck-overview-buffer)
        (pi-deck-overview-mouse-1
         (pi-deck-test--click (selected-window)
                              (+ 4 (next-single-property-change (point-min) 'pi-deck-space)))))
      (should (equal (pi-deck-current-space) "space")))))

(ert-deftest pi-deck-test-sidebar-default-and-saved-toggle ()
  (pi-deck-test--with-deck
    (setq pi-deck-sidebar t)
    (pi-deck-create-space dir "space")
    (should (get-buffer-window pi-deck-sidebar-buffer-name))
    (pi-deck-new-tab "second")
    (should (get-buffer-window pi-deck-sidebar-buffer-name))
    (pi-deck-root-show)
    (should-not (get-buffer-window pi-deck-sidebar-buffer-name))
    (pi-deck-root)
    (pi-deck-sidebar-toggle)
    (should-not (get-buffer-window pi-deck-sidebar-buffer-name))
    (setq pi-deck-sidebar t)
    (pi-deck-load-preferences)
    (should-not pi-deck-sidebar)))

(ert-deftest pi-deck-test-key-sheet-lists-live-bindings ()
  (pi-deck-test--with-deck
    (let ((pi-deck-key-sheet t)
          (pi-deck-key-sheet-substitutions '((delete-other-windows . pi-deck-test-maximize)))
          (saved (keymap-global-lookup "C-c d"))
          ran)
      (keymap-global-set "C-c d" pi-deck-command-map)
      (keymap-global-set "C-c w m" #'pi-deck-test-maximize)
      (defalias 'pi-deck-test-maximize #'ignore)
      (unwind-protect
          (progn
            (pi-deck-key-sheet-invalidate)
            (pi-deck-root-show)
            (with-current-buffer (pi-deck-overview-buffer)
              (pi-deck-overview-refresh (current-buffer))
              (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "^Keys" text))
                (should (string-match-p "C-c d a +new agent" text))
                (should (string-match-p "C-c d 1…9 +tab N" text))
                (should (string-match-p "C-c d = +repair pairs" text))
                (should (string-match-p "C-c C-c +send" text))
                (should (string-match-p "0…9 +choice N" text))
                ;; A menu-only command is reached through the menu.
                (should (string-match-p "C-c d m D +restart daemon" text))
                ;; The substitution's key replaces `delete-other-windows'.
                (should (string-match-p "C-c w m +maximize" text)))
              ;; Cached: the same layout until keymaps change.
              (should (eq (pi-deck-key-sheet-string 79) (pi-deck-key-sheet-string 79)))
              (keymap-set pi-deck-command-map "a" nil)
              (keymap-set pi-deck-command-map "c" #'pi-deck-new-agent)
              (unwind-protect
                  (progn
                    (pi-deck-test--refresh-interactively)
                    (should (string-match-p "C-c d c +new agent" (buffer-string))))
                (keymap-set pi-deck-command-map "c" nil)
                (keymap-set pi-deck-command-map "a" #'pi-deck-new-agent))
              ;; Every line fits 80 columns.
              (goto-char (point-min))
              (pi-deck-key-sheet-invalidate)
              (pi-deck-overview-refresh (current-buffer))
              (dolist (line (split-string (pi-deck-key-sheet-string 79) "\n"))
                (should (<= (string-width line) 79)))
              ;; Clicking an entry runs its command.
              (let ((entry (text-property-any (point-min) (point-max)
                                              'pi-deck-key-command 'pi-deck-goto)))
                (should entry)
                (should (eq (keymap-lookup (get-text-property entry 'keymap) "<mouse-1>")
                            #'pi-deck-key-sheet-mouse-run))
                (cl-letf (((symbol-function 'pi-deck-goto)
                           (lambda () (interactive) (setq ran t))))
                  (pi-deck-key-sheet-mouse-run
                   (list 'mouse-1 (list (get-buffer-window) entry '(0 . 0) 0 nil
                                        entry '(0 . 0) nil '(0 . 0) '(1 . 1))))))
              (should ran)
              ;; `h' hides the sheet and saves the choice.
              (pi-deck-key-sheet-toggle)
              (should-not (string-match-p "^Keys" (buffer-string)))
              (setq pi-deck-key-sheet t)
              (pi-deck-load-preferences)
              (should-not pi-deck-key-sheet)))
        (keymap-global-set "C-c d" saved)
        (keymap-global-unset "C-c w m")
        (pi-deck-key-sheet-invalidate)))))

(ert-deftest pi-deck-test-show-session-uses-or-creates-its-space ()
  (pi-deck-test--with-deck
    (should (eq pi-orchestration-show-session-function #'pi-deck-show-session))
    (let ((other (file-name-as-directory (make-temp-file "pi-deck-switch-" t))))
      (cl-letf (((symbol-function 'pi-deck--client-frames) (lambda () (list (selected-frame))))
                ((symbol-function 'select-frame-set-input-focus) #'ignore))
        ;; A directory without a space gets one, with a tab holding the pair.
        (let ((agent (pi-deck-show-session (pi-deck-test--session other "switched"))))
          (should (equal (pi-deck-agent-assigned-space agent) (file-name-nondirectory (directory-file-name other))))
          (should (equal (pi-deck-current-space) (pi-deck-agent-assigned-space agent)))
          (should (eq (window-buffer) (pilish-session-input-buffer (pi-deck-agent-buffer agent))))
          (should (get-buffer-window (pi-deck-agent-buffer agent))))
        ;; An existing space for the directory gets a new tab, from the root too.
        (pi-deck-create-space dir "space")
        (pi-deck-root-show)
        (let* ((tabs (length (pi-deck--space-tab-entries "space")))
               (agent (pi-deck-show-session (pi-deck-test--session dir "second"))))
          (should (equal (pi-deck-agent-assigned-space agent) "space"))
          (should (equal (pi-deck-current-space) "space"))
          (should (= (length (pi-deck--space-tab-entries "space")) (1+ tabs)))
          (should (eq (window-buffer) (pilish-session-input-buffer (pi-deck-agent-buffer agent))))
          ;; Showing it again selects its tab instead of adding one.
          (pi-deck-root-show)
          (pi-deck-show-session (pi-deck-agent-buffer agent))
          (should (= (length (pi-deck--space-tab-entries "space")) (1+ tabs)))))
      ;; Without a client frame, the next frame shows it.
      (cl-letf (((symbol-function 'pi-deck--client-frames) (lambda () nil)))
        (let ((agent (pi-deck-show-session (pi-deck-test--session other "later"))))
          (should (eq pi-deck--pending-reveal (pi-deck-agent-buffer agent)))
          (pi-deck-root-show)))
      (cl-letf (((symbol-function 'pi-deck--client-frames) (lambda () (list (selected-frame)))))
        (pi-deck-setup-frame (selected-frame)))
      (should-not pi-deck--pending-reveal)
      (should (string-match-p "later" (buffer-name (window-buffer))))
      (delete-directory other t))))

(ert-deftest pi-deck-test-new-frame-lands-on-root ()
  (pi-deck-test--with-deck
    (switch-to-buffer (pi-deck-initial-buffer))
    (should (equal (buffer-name) pi-deck-overview-buffer-name))
    (pi-deck-land-on-root)
    (should (pi-deck-root-tab-p (tab-bar--current-tab-find)))
    (should (= 1 (length (pi-deck--tabs))))))

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

(defun pi-deck-test--refresh-interactively ()
  "Refresh the current overview as \\[pi-deck-overview-refresh] does."
  (funcall-interactively #'pi-deck-overview-refresh))

(provide 'pi-deck-test)
;;; pi-deck-test.el ends here
