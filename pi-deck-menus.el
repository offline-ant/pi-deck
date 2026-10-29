;;; pi-deck-menus.el --- Popup and context menus -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Menus opened by mouse: the tab bar's space button (switch space), its `+'
;; (new tab), and right-click menus on agents, closed workers, spaces and
;; tabs.  They are ordinary menu keymaps shown with `popup-menu', which text
;; terminals render as text menus, so everything works by mouse in TTY and
;; GUI frames alike.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-close)
(require 'pi-deck-workers)

(declare-function pi-deck-jump "pi-deck-goto" (agent))
(declare-function pi-deck-answer-dialog "pi-deck-goto" (&optional agent))
(declare-function pi-deck-root-show "pi-deck-overview" ())
(declare-function pi-deck-zoom "pi-deck-spaces" ())

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
  "Return a function selecting SPACE's current tab, then calling FUNCTION."
  (lambda ()
    (pi-deck-select-space space)
    (funcall function)))

(defun pi-deck-space-menu (space)
  "Return the context menu of SPACE."
  (pi-deck--menu
   (format "Space %s" space)
   (list (list "New agent" (pi-deck--in-space space (lambda () (pi-deck-new-agent nil))))
         (list "New shell" (pi-deck--in-space space #'pi-deck-new-shell))
         (list "New tab" (pi-deck--in-space space #'pi-deck-new-tab))
         nil
         (list "Rename…" (pi-deck--in-space space (lambda () (call-interactively #'pi-deck-rename-space))))
         (list "Close space" (lambda () (pi-deck-close-space space last-nonmenu-event))))))

(defun pi-deck-spaces-menu (&optional event)
  "Pop up a menu of spaces at EVENT: switch to one, or create a space."
  (interactive (list last-nonmenu-event))
  (let ((statuses (pi-deck-space-statuses)))
    (pi-deck-popup
     (pi-deck--menu
      "Spaces"
      (append
       (mapcar (lambda (space)
                 (let ((name (plist-get space :name)))
                   (list (format "%s %s" (alist-get (or (cdr (assoc name statuses)) 'idle)
                                                    pi-deck-status-icons)
                                 name)
                         (lambda () (pi-deck-select-space name)))))
               pi-deck-spaces)
       (list nil
             (list "New space…" (lambda () (call-interactively #'pi-deck-create-space)))
             (list "Overview" #'pi-deck-root-show))))
     event)))

;;;; Tabs

(defun pi-deck--new-shell-tab ()
  "Open a new tab in the current space with a shell."
  (pi-deck--new-tab-in-space (pi-deck--require-space))
  (pi-deck-new-shell))

(defun pi-deck--new-tab-menu-map ()
  "Return the menu of `+' in the tab bar."
  (pi-deck--menu
   "New tab"
   (list (list "New agent" (lambda () (pi-deck--enter-pane-space) (pi-deck-new-agent nil)))
         (list "New shell" (lambda () (pi-deck--enter-pane-space) (pi-deck--new-shell-tab)))
         (list "Empty tab" (lambda () (pi-deck--enter-pane-space) (pi-deck-new-tab nil))))))

(defun pi-deck-new-tab-menu (&optional event)
  "Pop up the new tab menu at EVENT: new agent, shell or empty tab."
  (interactive (list last-nonmenu-event))
  (pi-deck-popup (pi-deck--new-tab-menu-map) event))

(defun pi-deck-tab-context-menu (number)
  "Return the context menu of the tab numbered NUMBER in the selected frame."
  (let ((tab (nth (1- number) (pi-deck--tabs))))
    (pi-deck--menu
     (format "Tab %s" (alist-get 'name tab))
     (list (list "Rename…" (lambda ()
                             (tab-bar-rename-tab (read-string "Tab name: " (alist-get 'name tab))
                                                 number)))
           (list "Close" (lambda () (pi-deck-close-tab number)))))))

;;;; Agents

(defun pi-deck-rename-agent (agent name)
  "Rename AGENT to NAME: a Pi session's name, or a terminal's buffer."
  (if (eq (pi-deck-agent-kind agent) 'pi)
      (with-current-buffer (pi-deck-agent-buffer agent)
        (pilish-set-session-name name))
    (with-current-buffer (pi-deck-agent-buffer agent)
      (rename-buffer (format "*%s*" name) t)))
  (pi-deck--schedule-update))

(defun pi-deck--zoom-agent (agent)
  "Jump to AGENT and zoom its pane."
  (pi-deck-jump agent)
  (pi-deck-zoom))

(defun pi-deck-agent-menu (agent)
  "Return the context menu of AGENT."
  (pi-deck--menu
   (pi-deck-agent-name agent)
   (list (list "Jump" (lambda () (pi-deck-jump agent)))
         (list "Show beside" (lambda () (pi-deck-show-beside agent)))
         (list "Zoom" (lambda () (pi-deck--zoom-agent agent)))
         (list "Rename…" (lambda ()
                           (pi-deck-rename-agent agent (read-string "Name: " (pi-deck-agent-name agent)))))
         (list "Answer question" (lambda () (pi-deck-answer-dialog agent))
               (pi-deck-agent-dialogs agent))
         nil
         (list "Stop" (lambda () (pi-deck-stop agent last-nonmenu-event))))))

(defun pi-deck-finished-worker-menu (record)
  "Return the context menu of the closed worker RECORD."
  (pi-deck--menu
   (plist-get record :name)
   (list (list "Open transcript" (lambda () (pi-deck-open-finished-worker record))))))

(provide 'pi-deck-menus)
;;; pi-deck-menus.el ends here
