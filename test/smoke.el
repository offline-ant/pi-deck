;;; smoke.el --- Real Pi smoke test inside a disposable daemon -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Loaded by test/smoke.sh into `emacs -Q --daemon=<disposable>'.  Phase 1
;; drives a real shell pane (the user's default shell with ghostel's shell
;; integration) through idle, working and exit, then a real Pi RPC session
;; (faux provider only) through working, done, blocked and answered states
;; and saves the deck.  With a client frame, the agent opens panels: beside
;; it when it is on screen, only listed in the sidebar when hidden.
;; test/smoke.sh then
;; restarts the daemon through `pi-deck-restart-daemon'; phase 2 checks that
;; the space and session were restored from the session file, with the
;; Emacs orchestration environment, with the space selected last, and
;; still work.  Phase 3 checks the same
;; after a plain `kill-emacs' and a fresh daemon start.

;;; Code:

(require 'pi-deck)

(defvar pi-deck-smoke-notifications nil
  "Notifications delivered during the smoke test, newest first.")

(setq pi-deck-notify-function
      (lambda (title _body _agent) (push title pi-deck-smoke-notifications)))

(defun pi-deck-smoke--wait (what predicate &optional seconds)
  "Process events until PREDICATE holds; signal an error naming WHAT on timeout."
  (let ((deadline (+ (float-time) (or seconds 60))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (pi-deck-refresh))
    (or (funcall predicate) (error "Timed out waiting for %s" what))))

(defun pi-deck-smoke--submit (chat text)
  "Submit TEXT to CHAT and wait until Pi accepted it."
  (let (response)
    (pilish-submit-prompt chat text (lambda (r) (setq response r)))
    (pi-deck-smoke--wait (format "acceptance of %S" text) (lambda () response))
    (unless (eq (plist-get response :success) t)
      (error "Prompt %S rejected: %S" text response))))

(defun pi-deck-smoke--chat-text (chat)
  "Return CHAT's text."
  (with-current-buffer chat (buffer-substring-no-properties (point-min) (point-max))))

(defun pi-deck-smoke--shell ()
  "Drive a real default-shell pane through prompt, command and exit."
  (let* ((shell (pi-deck-new-shell))
         (buffer (pi-deck-agent-buffer shell)))
    (pi-deck-smoke--wait "shell prompt" (lambda () (eq (pi-deck-agent-state shell) 'idle)) 30)
    (with-current-buffer buffer (ghostel-send-string "sleep 1\n"))
    (pi-deck-smoke--wait "running command" (lambda () (eq (pi-deck-agent-state shell) 'working)) 10)
    (pi-deck-smoke--wait "finished command" (lambda () (eq (pi-deck-agent-state shell) 'idle)) 10)
    (with-current-buffer buffer (ghostel-send-string "exit 3\n"))
    (pi-deck-smoke--wait "shell exit" (lambda () (equal (pi-deck-agent-detail shell) "exited 3")) 10)
    (pi-deck-stop-agent shell)))

(defun pi-deck-smoke-phase1 (project)
  "Exercise a real session in a space for PROJECT; return its session file."
  (pi-deck-create-space project "smoke")
  (pi-deck-smoke--shell)
  (let* ((agent (pi-deck-new-agent "main"))
         (chat (pi-deck-agent-buffer agent))
         ready)
    (pi-deck-smoke--wait "Pi startup"
                         (lambda () (setq ready (plist-get (buffer-local-value 'pilish--state chat)
                                                           :session-file))))
    (unless (equal (pi-deck-agent-model agent) "one")
      (error "Unexpected model %S" (pi-deck-agent-model agent)))
    ;; Hide the session so finishing makes it done and notifies.
    (pi-deck-hide-pane (get-buffer-window chat))
    (when (get-buffer-window chat) (error "Hiding left the session on screen"))
    (pi-deck-smoke--submit chat "hello deck")
    (pi-deck-smoke--wait "a finished reply"
                         (lambda () (and (eq (pi-deck-agent-state agent) 'idle)
                                         (string-match-p "faux reply: hello deck"
                                                         (pi-deck-smoke--chat-text chat)))))
    (unless (eq (pi-deck-agent-status agent) 'done) (error "Not done after unseen reply"))
    (unless (member "main finished" pi-deck-smoke-notifications)
      (error "No finished notification: %S" pi-deck-smoke-notifications))
    ;; A real extension dialog blocks the agent until answered.
    (pilish-submit-prompt chat "/deck-confirm" #'ignore)
    (pi-deck-smoke--wait "blocked dialog" (lambda () (eq (pi-deck-agent-status agent) 'blocked)))
    (unless (equal (pi-deck-agent-detail agent) "asks: Deck check")
      (error "Unexpected detail %S" (pi-deck-agent-detail agent)))
    (unless (member "main needs attention" pi-deck-smoke-notifications)
      (error "No attention notification: %S" pi-deck-smoke-notifications))
    (with-current-buffer (pi-deck-sidebar-buffer)
      (pi-deck-sidebar-refresh)
      (unless (string-match-p "◉ main" (buffer-string))
        (error "Sidebar lacks the blocked agent:\n%s" (buffer-string))))
    ;; Jumping focuses the inline question; pressing its Yes button answers.
    (pi-deck-jump agent)
    (with-current-buffer chat
      (unless (eq (get-text-property (point) 'pilish-dialog-choice) 0)
        (error "Jump did not focus the inline dialog"))
      (push-button (point)))
    (pi-deck-smoke--wait "answered dialog" (lambda () (null (pi-deck-agent-dialogs agent))))
    (pi-deck-smoke--wait "idle after dialog" (lambda () (eq (pi-deck-agent-state agent) 'idle)))
    (pi-deck-save-state)
    (format "PASS1 %s" (plist-get (buffer-local-value 'pilish--state chat) :session-file))))

(defun pi-deck-smoke--panel-buffer (name)
  "Return the live orchestration panel buffer named NAME, or nil."
  (cl-find-if (lambda (buffer) (string-prefix-p (format "pi-panel:%s:" name) (buffer-name buffer)))
              (buffer-list)))

(defun pi-deck-smoke-panels (suffix)
  "Let the real agent open panels; check pi-deck shows them in a client frame.
Panel names end in SUFFIX: Pi's panel names are global to the machine."
  (let* ((beside (concat "beside-" suffix))
         (aside (concat "aside-" suffix))
         (frame (or (car (pi-deck--client-frames)) (error "No client frame")))
         (agent (or (cl-find-if (lambda (agent) (equal (pi-deck-agent-session-name agent) "main"))
                                (pi-deck-agents))
                    (error "No main agent")))
         (chat (pi-deck-agent-buffer agent)))
    (with-selected-frame frame
      ;; The parent is on screen and its pane is wide: the panel opens to its
      ;; right, unfocused.
      (pi-deck-reveal agent)
      (let ((focus (window-buffer (selected-window))))
        (pi-deck-smoke--submit chat (concat "open panel " beside))
        (pi-deck-smoke--wait "panel beside the agent"
                             (lambda () (let ((panel (pi-deck-smoke--panel-buffer beside)))
                                          (and panel (get-buffer-window panel frame)))))
        (let* ((panel (pi-deck-smoke--panel-buffer beside))
               (window (get-buffer-window panel frame)))
          (unless (eq (window-buffer (selected-window)) focus) (error "The panel took focus"))
          (unless (< (window-pixel-left (get-buffer-window chat frame)) (window-pixel-left window))
            (error "The panel is not right of its agent"))
          (unless (eq (pi-deck-agent-parent (pi-deck-agent panel)) chat)
            (error "The panel is not linked to its agent"))
          (with-current-buffer (pi-deck-sidebar-buffer)
            (pi-deck-sidebar-refresh)
            (unless (string-match-p (concat "main[^\n]*✕\n +[^\n]*" beside "[^\n]*✕")
                                    (buffer-string))
              (error "The sidebar lacks the panel under its agent:\n%s" (buffer-string)))))
        (pi-deck-smoke--wait "turn end" (lambda () (eq (pi-deck-agent-state agent) 'idle)))
        (pi-deck-smoke--submit chat (concat "close panel " beside))
        (pi-deck-smoke--wait "panel window removed"
                             (lambda () (and (null (pi-deck-smoke--panel-buffer beside))
                                             (not (cl-find-if
                                                   (lambda (window)
                                                     (string-prefix-p "pi-panel:" (buffer-name (window-buffer window))))
                                                   (window-list frame 'no-minibuf)))))))
      (pi-deck-smoke--wait "turn end" (lambda () (eq (pi-deck-agent-state agent) 'idle)))
      ;; The parent is hidden: the panel is only listed, in its space.
      (pi-deck-hide-pane (get-buffer-window chat frame))
      (let ((tabs (length (pi-deck--tabs frame)))
            (windows (window-list frame 'no-minibuf)))
        (pi-deck-smoke--submit chat (concat "open panel " aside))
        (pi-deck-smoke--wait "hidden panel" (lambda () (pi-deck-smoke--panel-buffer aside)))
        (pi-deck-smoke--wait "turn end" (lambda () (eq (pi-deck-agent-state agent) 'idle)))
        (let ((panel (pi-deck-smoke--panel-buffer aside)))
          (when (get-buffer-window panel frame) (error "The hidden agent's panel was shown"))
          (unless (equal (window-list frame 'no-minibuf) windows)
            (error "The hidden agent's panel changed the layout"))
          (unless (= (length (pi-deck--tabs frame)) tabs) (error "The panel added a tab"))
          (unless (equal (pi-deck-agent-space (pi-deck-agent panel)) "smoke")
            (error "The panel is not in the agent's space")))
        (pi-deck-smoke--submit chat (concat "close panel " aside))
        (pi-deck-smoke--wait "hidden panel closed"
                             (lambda () (null (pi-deck-smoke--panel-buffer aside)))))
      (pi-deck-smoke--wait "turn end" (lambda () (eq (pi-deck-agent-state agent) 'idle))))
    "PASS-PANELS"))

(defvar pi-deck-smoke-panels-result nil
  "Outcome of `pi-deck-smoke-start-panels': nil while running, then a string.")

(defun pi-deck-smoke-start-panels (suffix)
  "Run `pi-deck-smoke-panels' with SUFFIX from a timer; poll the result.
The panel tools reach Emacs through emacsclient, which this server cannot
answer while it is still evaluating the request that runs the check."
  (setq pi-deck-smoke-panels-result nil)
  (run-at-time 0 nil (lambda ()
                       (setq pi-deck-smoke-panels-result
                             (condition-case err (pi-deck-smoke-panels suffix)
                               (error (error-message-string err)))))))

(defun pi-deck-smoke--restored-agent (session-file history)
  "Return the restored main agent once it shows HISTORY from SESSION-FILE.
Checks that every restored Pi process got the Emacs orchestration
environment, which needs the server running before the restore."
  (pi-deck-smoke--wait "restore to finish" (lambda () (null pi-deck--restoring)))
  (unless (pi-deck-space "smoke") (error "Space was not restored"))
  (unless (equal pi-deck--last-space "smoke") (error "The current space was not restored"))
  (let* ((agent (or (cl-find-if (lambda (agent) (equal (pi-deck-agent-session-name agent) "main"))
                                (pi-deck-agents))
                    (error "Session was not restored: %S" (mapcar #'pi-deck-agent-name (pi-deck-agents)))))
         (chat (pi-deck-agent-buffer agent)))
    (pi-deck-smoke--wait "restored history"
                         (lambda () (string-match-p history (pi-deck-smoke--chat-text chat))))
    (pi-deck-smoke--wait "restored session file"
                         (lambda () (equal (pi-deck-agent-session-file agent) session-file)))
    (unless (equal (pi-deck-agent-assigned-space agent) "smoke")
      (error "Restored agent lost its space"))
    ;; Every restored session, including the first (started at once).
    (dolist (other (pi-deck-agents))
      (when (eq (pi-deck-agent-kind other) 'pi)
        (let ((environ (with-temp-buffer
                         (insert-file-contents-literally
                          (format "/proc/%d/environ" (process-id (pi-deck-agent-process other))))
                         (split-string (buffer-string) "\0" t))))
          (unless (member "PI_ORCHESTRATION_HOST=emacs" environ)
            (error "Restored session %s lacks the Emacs orchestration environment"
                   (pi-deck-agent-name other))))))
    agent))

(defun pi-deck-smoke-phase3 (session-file)
  "Check a daemon started after a plain `kill-emacs' restored SESSION-FILE."
  (pi-deck-smoke--restored-agent session-file "faux reply: after restart")
  (format "PASS3 %d agents" (length (pi-deck-agents))))

(defun pi-deck-smoke-phase2 (session-file)
  "Check the restored deck resumed SESSION-FILE and still answers."
  (let* ((agent (pi-deck-smoke--restored-agent session-file "faux reply: hello deck"))
         (chat (pi-deck-agent-buffer agent)))
    (pi-deck-smoke--submit chat "after restart")
    (pi-deck-smoke--wait "reply after restart"
                         (lambda () (and (eq (pi-deck-agent-state agent) 'idle)
                                         (string-match-p "faux reply: after restart"
                                                         (pi-deck-smoke--chat-text chat)))))
    (format "PASS2 %d agents" (length (pi-deck-agents)))))

(provide 'smoke)
;;; smoke.el ends here
