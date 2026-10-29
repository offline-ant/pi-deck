;;; pi-deck-layout.el --- Grid of the current space's agents -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `pi-deck-layout-grid' arranges the current space's agents in a grid.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)

(defcustom pi-deck-grid-include-shells nil
  "Whether the agents grid also shows the space's shells."
  :type 'boolean
  :group 'pi-deck)

(defun pi-deck--grid (agents)
  "Show AGENTS in a balanced grid filling the current layout.
Each Pi agent's cell holds its chat over its input."
  (pi-deck--single-window)
  (let* ((count (length agents))
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
               for agent = (pop agents)
               do (if agent (pi-deck--display-agent-in-window agent window)
                    (delete-window window))))))

;;;###autoload
(defun pi-deck-layout-grid ()
  "Show all of the space's agents in a grid, Pi agents with their inputs."
  (interactive)
  (pi-deck--enter-pane-space)
  (let ((agents (cl-remove-if-not
                 (lambda (agent) (or pi-deck-grid-include-shells (eq (pi-deck-agent-kind agent) 'pi)))
                 (pi-deck--space-agents (pi-deck-current-space)))))
    (unless agents (user-error "No agents in this space"))
    (pi-deck--grid agents)))

(provide 'pi-deck-layout)
;;; pi-deck-layout.el ends here
