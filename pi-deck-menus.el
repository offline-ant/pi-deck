;;; pi-deck-menus.el --- Popup and context menus -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Menus opened by mouse: a space's `+' and right-click menus on agents,
;; closed workers and spaces.  They are ordinary menu keymaps shown with
;; `popup-menu', which text terminals render as text menus, so everything
;; works by mouse in TTY and GUI frames alike.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-close)
(require 'pi-deck-workers)

(declare-function pi-deck-jump "pi-deck-goto" (agent))
(declare-function pi-deck-answer-dialog "pi-deck-goto" (&optional agent))
(declare-function pi-deck-show-only "pi-deck-goto" (agent))
(declare-function pi-deck--current-agent "pi-deck-goto" ())

(defun pi-deck--menu (title entries)
  "Return a menu keymap titled TITLE with ENTRIES.
Each entry is (LABEL FUNCTION) or (LABEL FUNCTION ENABLE); FUNCTION takes
no arguments.  A nil entry is a separator; entries with ENABLE nil are
omitted."
  (let ((menu (make-sparse-keymap title)) (n 0))
    (dolist (entry entries)
      (cl-incf n)
      (cond
       ((null entry)
        (define-key-after menu (vector (intern (format "sep-%d" n))) '(menu-item "--")))
       ((or (< (length entry) 3) (nth 2 entry))
        (let ((function (nth 1 entry)))
          (define-key-after menu (vector (intern (format "item-%d" n)))
            `(menu-item ,(car entry) ,(lambda () (interactive) (funcall function))))))))
    menu))

(defun pi-deck-popup (menu &optional event)
  "Show MENU at EVENT's position (else the mouse) and run the chosen item."
  (popup-menu menu (or event last-nonmenu-event)))

;;;; Spaces

(defun pi-deck--in-space (space function)
  "Return a function showing SPACE's layout, then calling FUNCTION."
  (lambda ()
    (pi-deck-select-space space)
    (funcall function)))

(defun pi-deck-space-menu (space)
  "Return the context menu of SPACE."
  (pi-deck--menu
   (format "Space %s" space)
   (list (list "New agent" (pi-deck--in-space space (lambda () (pi-deck-new-agent nil))))
         (list "New shell" (pi-deck--in-space space #'pi-deck-new-shell))
         nil
         (list "Rename…" (pi-deck--in-space space (lambda () (call-interactively #'pi-deck-rename-space))))
         (list "Close space" (lambda () (pi-deck-close-space space last-nonmenu-event))))))

;;;; Agents

(defun pi-deck-fork-agent (agent)
  "Open an independent fork of the Pi AGENT, listed in its space.
Pi's `/fork-here' names it after the session; the draft is untouched and
a busy agent keeps working."
  (unless (eq (pi-deck-agent-kind agent) 'pi)
    (user-error "Only Pi agents can be forked"))
  (let ((name (pi-deck-agent-name agent)))
    (pilish-submit-prompt
     (pi-deck-agent-buffer agent) "/fork-here"
     (lambda (response)
       (unless (eq (plist-get response :success) t)
         (message "pi-deck: forking %s failed: %s" name (plist-get response :error)))))
    (message "pi-deck: forking %s…" name)))

(defun pi-deck-fork (&optional agent)
  "Fork AGENT, by default the Pi agent shown in the selected pane."
  (interactive)
  (let ((agent (or agent (pi-deck--current-agent))))
    (unless agent (user-error "No agent in this pane"))
    (pi-deck-fork-agent agent)))

(defun pi-deck--agent-busy-p (agent)
  "Return non-nil when the Pi AGENT is working or asking."
  (memq (pi-deck-agent-state agent) '(working blocked)))

(defun pi-deck--copy-session-file (agent)
  "Copy the Pi AGENT's session file name to the kill ring."
  (if-let* ((file (pilish-session-file (pi-deck-agent-buffer agent))))
      (progn (kill-new file) (message "Copied %s" file))
    (message "pi-deck: %s has no session file yet" (pi-deck-agent-name agent))))

(defun pi-deck-rename-agent (agent name)
  "Rename AGENT to NAME: a Pi session's name, or a terminal's buffer."
  (if (eq (pi-deck-agent-kind agent) 'pi)
      (with-current-buffer (pi-deck-agent-buffer agent)
        (pilish-set-session-name name))
    (with-current-buffer (pi-deck-agent-buffer agent)
      (rename-buffer (format "*%s*" name) t)))
  (pi-deck--schedule-update))

(defun pi-deck-agent-menu (agent)
  "Return the context menu of AGENT."
  (pi-deck--menu
   (pi-deck-agent-name agent)
   (list (list "Jump" (lambda () (pi-deck-jump agent)))
         (list "Show beside" (lambda () (pi-deck-show-beside agent)))
         (list "Show alone" (lambda () (pi-deck-show-only agent)))
         (list "Rename…" (lambda ()
                           (pi-deck-rename-agent agent (read-string "Name: " (pi-deck-agent-name agent)))))
         (list "Answer question" (lambda () (pi-deck-answer-dialog agent))
               (pi-deck-agent-dialogs agent))
         nil
         (list "Fork" (lambda () (pi-deck-fork-agent agent)) (eq (pi-deck-agent-kind agent) 'pi))
         (list "Stop turn" (lambda () (with-current-buffer (pi-deck-agent-buffer agent) (pilish-abort)))
               (and (eq (pi-deck-agent-kind agent) 'pi) (pi-deck--agent-busy-p agent)))
         (list "Compact" (lambda () (with-current-buffer (pi-deck-agent-buffer agent) (pilish-compact)))
               (and (eq (pi-deck-agent-kind agent) 'pi) (not (pi-deck--agent-busy-p agent))))
         (list "Copy session file" (lambda () (pi-deck--copy-session-file agent))
               (eq (pi-deck-agent-kind agent) 'pi))
         nil
         (list "Stop" (lambda () (pi-deck-stop agent last-nonmenu-event))))))

(defun pi-deck-finished-worker-menu (record)
  "Return the context menu of the closed worker RECORD."
  (pi-deck--menu
   (plist-get record :name)
   (list (list "Open transcript" (lambda () (pi-deck-open-finished-worker record)))
         (list "Resume" (lambda () (pi-deck-resume-finished-worker record))))))

(provide 'pi-deck-menus)
;;; pi-deck-menus.el ends here
