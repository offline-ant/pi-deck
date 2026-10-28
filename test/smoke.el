;;; smoke.el --- Real Pi smoke test inside a disposable daemon -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Loaded by test/smoke.sh into `emacs -Q --daemon=<disposable>'.  Phase 1
;; drives a real Pi RPC session (faux provider only) through working, done,
;; blocked and answered states and saves the deck.  test/smoke.sh then
;; restarts the daemon through `pi-deck-restart-daemon'; phase 2 checks that
;; the space and session were restored from the session file and still work.

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

(defun pi-deck-smoke-phase1 (project)
  "Exercise a real session in a space for PROJECT; return its session file."
  (pi-deck-create-space project "smoke")
  (let* ((agent (pi-deck-new-agent "main"))
         (chat (pi-deck-agent-buffer agent))
         ready)
    (pi-deck-smoke--wait "Pi startup"
                         (lambda () (setq ready (plist-get (buffer-local-value 'pilish--state chat)
                                                           :session-file))))
    (unless (equal (pi-deck-agent-model agent) "one")
      (error "Unexpected model %S" (pi-deck-agent-model agent)))
    ;; Hide the session so finishing makes it done and notifies.
    (pi-deck-root-show)
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
    (with-current-buffer (pi-deck-overview-buffer)
      (pi-deck-overview-refresh)
      (unless (string-match-p "◉ blocked  main" (buffer-string))
        (error "Overview lacks the blocked agent:\n%s" (buffer-string))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (pi-deck-jump agent))
    (pi-deck-smoke--wait "answered dialog" (lambda () (null (pi-deck-agent-dialogs agent))))
    (pi-deck-smoke--wait "idle after dialog" (lambda () (eq (pi-deck-agent-state agent) 'idle)))
    (pi-deck-save-state)
    (format "PASS1 %s" (plist-get (buffer-local-value 'pilish--state chat) :session-file))))

(defun pi-deck-smoke-phase2 (session-file)
  "Check the restored deck resumed SESSION-FILE and still answers."
  (unless (pi-deck-space "smoke") (error "Space was not restored"))
  (let* ((agent (or (cl-find-if (lambda (agent) (equal (pi-deck-agent-session-name agent) "main"))
                                (pi-deck-agents))
                    (error "Session was not restored: %S" (mapcar #'pi-deck-agent-name (pi-deck-agents)))))
         (chat (pi-deck-agent-buffer agent)))
    (pi-deck-smoke--wait "restored history"
                         (lambda () (string-match-p "faux reply: hello deck"
                                                    (pi-deck-smoke--chat-text chat))))
    (pi-deck-smoke--wait "restored session file"
                         (lambda () (equal (pi-deck-agent-session-file agent) session-file)))
    (unless (equal (pi-deck-agent-assigned-space agent) "smoke")
      (error "Restored agent lost its space"))
    (pi-deck-smoke--submit chat "after restart")
    (pi-deck-smoke--wait "reply after restart"
                         (lambda () (and (eq (pi-deck-agent-state agent) 'idle)
                                         (string-match-p "faux reply: after restart"
                                                         (pi-deck-smoke--chat-text chat)))))
    (format "PASS2 %d agents" (length (pi-deck-agents)))))

(provide 'smoke)
;;; smoke.el ends here
