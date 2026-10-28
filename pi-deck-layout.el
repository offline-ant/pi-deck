;;; pi-deck-layout.el --- Layout presets for the current tab -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Presets rearrange the current tab's windows around the space's agents:
;; one chat, chat beside a shell, a grid of all agents, or an agent with its
;; orchestration workers.  `pi-deck-layouts' maps names to functions, so
;; users can add their own.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)

(declare-function pi-deck--current-agent "pi-deck-goto" ())

(defcustom pi-deck-layouts
  '(("single" . pi-deck-layout-single)
    ("chat+shell" . pi-deck-layout-chat-shell)
    ("grid" . pi-deck-layout-grid)
    ("workers" . pi-deck-layout-workers))
  "Layout presets: names and functions arranging the current tab."
  :type '(alist :key-type string :value-type function)
  :group 'pi-deck)

(defcustom pi-deck-grid-include-shells nil
  "Whether the agents grid also shows the space's shells."
  :type 'boolean
  :group 'pi-deck)

(defun pi-deck--layout-space-agents (&optional kind)
  "Return the current space's agents, optionally only of KIND, oldest first."
  (let* ((space (pi-deck-current-space))
         (tab-index (pi-deck-tab-index)))
    (cl-remove-if-not (lambda (agent)
                        (and (equal (pi-deck-agent-space agent tab-index) space)
                             (or (null kind) (eq (pi-deck-agent-kind agent) kind))))
                      (pi-deck-agents))))

(defun pi-deck--layout-primary ()
  "Return the agent a layout centres on.
That is the selected window's agent, else the space's first Pi agent."
  (or (pi-deck--current-agent)
      (car (pi-deck--layout-space-agents 'pi))
      (user-error "No Pi agent in this space")))

(defun pi-deck-layout-single ()
  "Show only the primary agent with its input."
  (let ((agent (pi-deck--layout-primary)))
    (delete-other-windows)
    (pi-deck--display-agent-in-window agent (selected-window))
    (select-window (pi-deck--input-or-chat-window agent))))

(defun pi-deck-layout-chat-shell ()
  "Show the primary Pi agent on the left and a shell of the space on the right."
  (let* ((agent (let ((current (pi-deck--layout-primary)))
                  (if (eq (pi-deck-agent-kind current) 'pi) current
                    (or (car (pi-deck--layout-space-agents 'pi))
                        (user-error "No Pi agent in this space")))))
         (shell (or (car (pi-deck--layout-space-agents 'shell))
                    (save-window-excursion (pi-deck-new-shell)))))
    (delete-other-windows)
    (let ((right (split-window-right)))
      (set-window-buffer right (pi-deck-agent-buffer shell))
      (pi-deck--display-agent-in-window agent (selected-window))
      (select-window (pi-deck--input-or-chat-window agent)))))

(defun pi-deck--grid (buffers)
  "Show BUFFERS in a balanced grid filling the current tab."
  (delete-other-windows)
  (let* ((count (length buffers))
         (columns (max 1 (ceiling (sqrt count))))
         (rows (max 1 (ceiling count (float columns))))
         (column-windows (list (selected-window))))
    (dotimes (_ (1- columns))
      (push (split-window (car column-windows) nil 'right) column-windows))
    (setq column-windows (nreverse column-windows))
    (let ((windows nil))
      (dolist (column column-windows)
        (let ((cells (list column)))
          (dotimes (_ (1- rows))
            (push (split-window (car cells) nil 'below) cells))
          (setq windows (append windows (nreverse cells)))))
      (balance-windows)
      (cl-loop for window in windows
               for buffer = (pop buffers)
               do (if buffer (set-window-buffer window buffer) (delete-window window))))))

(defun pi-deck-layout-grid ()
  "Show all of the space's agents (chats only) in a grid."
  (let ((agents (pi-deck--layout-space-agents (unless pi-deck-grid-include-shells 'pi))))
    (unless agents (user-error "No agents in this space"))
    (pi-deck--grid (mapcar #'pi-deck-agent-buffer agents))))

(defun pi-deck-layout-workers ()
  "Show the primary agent on the left and its workers stacked on the right."
  (let* ((agent (pi-deck--layout-primary))
         (workers (cl-remove-if-not
                   (lambda (other) (eq (pi-deck-agent-parent other) (pi-deck-agent-buffer agent)))
                   (pi-deck-agents))))
    (delete-other-windows)
    (when workers
      (let ((window (split-window-right)))
        (set-window-buffer window (pi-deck-agent-buffer (car workers)))
        (dolist (worker (cdr workers))
          (setq window (split-window window nil 'below))
          (set-window-buffer window (pi-deck-agent-buffer worker)))
        (balance-windows (window-parent window))))
    (pi-deck--display-agent-in-window agent (selected-window))
    (select-window (pi-deck--input-or-chat-window agent))))

;;;###autoload
(defun pi-deck-apply-layout (name)
  "Apply the layout preset NAME from `pi-deck-layouts' to the current tab.
From the root tab, switch to the most recently used tab and apply it there."
  (interactive (list (completing-read "Layout: " pi-deck-layouts nil t)))
  (let ((function (or (cdr (assoc name pi-deck-layouts))
                      (user-error "Unknown layout %s" name))))
    (when (pi-deck-root-tab-p (tab-bar--current-tab-find))
      (unless (cdr (pi-deck--tabs)) (user-error "No space tab to arrange"))
      (tab-bar-switch-to-recent-tab))
    (funcall function)))

(provide 'pi-deck-layout)
;;; pi-deck-layout.el ends here
