;;; pi-deck-close.el --- Hiding panes, stopping agents, closing spaces -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Closing a pane only hides it: its agent keeps running and stays in the
;; sidebar.  Agents stop explicitly: the sidebar's ✕, the agent menu's Stop,
;; or closing their space.  Stopping an agent also stops its workers and
;; panels.
;;
;; Stopping asks first only when something it would stop is working or
;; blocked, naming what will stop.  Commands started by the mouse ask with
;; a popup dialog, so the mouse alone can answer; keyboard commands ask in
;; the minibuffer.  Afterwards the panes showing stopped agents go from
;; every space's layout in every client frame.

;;; Code:

(require 'tab-bar)
(require 'pi-deck-core)
(require 'pi-deck-spaces)

(declare-function pi-deck--client-frames "pi-deck-daemon" ())
(declare-function pi-orchestration-stop-target "pi-orchestration" (id))

(defvar pi-deck-ask-function #'pi-deck--ask
  "Function asking a yes/no QUESTION with a CHOICE label and the initiating EVENT.
It returns non-nil to go ahead.")

;;;; Views

(defun pi-deck--shown-by-p (agent buffers)
  "Return non-nil when one of BUFFERS shows AGENT."
  (cl-some (lambda (buffer) (memq buffer buffers)) (pi-deck-agent-windows-buffers agent)))

(defun pi-deck--agents-shown-by (buffers)
  "Return the agents shown by BUFFERS."
  (cl-remove-if-not (lambda (agent) (pi-deck--shown-by-p agent buffers)) (pi-deck-agents)))

(defun pi-deck--hide-window-pane (window)
  "Remove WINDOW's pane from its layout.
The layout's only pane shows the space's directory instead."
  (let ((pane (pi-deck--pane-of window)))
    (if (eq pane (window-main-window (window-frame window)))
        (with-selected-window (pilish-dissolve-window-pair window)
          (delete-other-windows)
          (switch-to-buffer (pi-deck--space-start-buffer
                             (pi-deck-current-space (window-frame window)))
                            t t))
      (delete-window pane))))

(defun pi-deck--remove-views-here (buffers)
  "Remove the panes showing BUFFERS from the selected frame's layout."
  (dolist (window (window-list nil 'no-minibuf))
    (when (and (window-live-p window)
               (memq (window-buffer window) buffers)
               (not (window-parameter window 'window-side)))
      (pi-deck--hide-window-pane window))))

(defun pi-deck--remove-views (buffers)
  "Remove the panes showing BUFFERS from every layout of every client frame.
Layouts of spaces not on screen are shown briefly to change them."
  (dolist (frame (pi-deck--client-frames))
    (with-selected-frame frame
      (pi-deck--remove-views-here buffers)
      (let ((current (1+ (tab-bar--current-tab-index))) (number 0))
        (dolist (tab (pi-deck--tabs))
          (cl-incf number)
          (when (and (not (eq (car tab) 'current-tab))
                     (cl-some (lambda (buffer) (memq buffer buffers))
                              (pi-deck--tab-buffers tab frame)))
            (tab-bar-select-tab number)
            (pi-deck--remove-views-here buffers)
            (tab-bar-select-tab current))))))
  ;; Remembered layouts of spaces no frame shows keep the buffers too; such
  ;; a layout comes back without them.
  (setq pi-deck--layouts
        (cl-remove-if (lambda (cell)
                        (cl-some (lambda (buffer)
                                   (member (buffer-name buffer)
                                           (ignore-errors (window-state-buffers (cdr cell)))))
                                 buffers))
                      pi-deck--layouts)))

(defun pi-deck-hide-pane (&optional window)
  "Hide WINDOW's pane (default the selected pane); its agent keeps running.
A chat hides with its input.  The layout's only pane shows the space's
directory instead."
  (interactive)
  (pi-deck--hide-window-pane (or window (pi-deck--pane-window))))

;;;; Asking

(defun pi-deck--busy (agents)
  "Return the AGENTS that are working or blocked."
  (cl-remove-if-not (lambda (agent) (memq (pi-deck-agent-status agent) '(working blocked)))
                    agents))

(defun pi-deck--describe (agents)
  "Describe AGENTS for a confirmation: names with their status."
  (mapconcat (lambda (agent)
               (format "%s (%s)" (pi-deck-agent-name agent) (pi-deck-agent-status agent)))
             agents ", "))

(defun pi-deck--mouse-initiated-p (event)
  "Return non-nil when EVENT or the current command came from the mouse."
  (or (and (consp event) (mouse-event-p event))
      (consp last-nonmenu-event)))

(defun pi-deck--ask (question choice &optional event)
  "Ask QUESTION; CHOICE labels going ahead.  Return non-nil to go ahead.
From the mouse (EVENT, or the current command), a popup dialog asks, which
text terminals show as a menu; otherwise the minibuffer asks.  Cancel comes
first: text terminal dialogs pick the highlighted item for a click anywhere,
and RET picks it too."
  (if (and (pi-deck--mouse-initiated-p event) (display-popup-menus-p))
      (x-popup-dialog t (list question (cons "Cancel" nil) (cons choice t)))
    (y-or-n-p (format "%s %s? " question choice))))

(defun pi-deck-confirm-stop (what agents verb &optional event)
  "Return non-nil when WHAT may go ahead and stop AGENTS.
Ask only when one of AGENTS is working or blocked; the question names all
of them.  VERB completes the choice label, as in \"Stop 2 and close\"."
  (or (null (pi-deck--busy agents))
      (funcall pi-deck-ask-function
               (format "%s stops %s." what (pi-deck--describe agents))
               (concat (format "Stop %d" (length agents)) (if verb (concat " and " verb) ""))
               event)))

;;;; Stopping

(defun pi-deck-stop-agent (agent)
  "Stop AGENT and kill its buffers without asking.
Orchestration targets stop through their host, so Pi learns they are gone."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (cond
     ((not (buffer-live-p buffer)))
     ((and (pi-deck-agent-target agent) (fboundp 'pi-orchestration-stop-target))
      (pi-orchestration-stop-target (pi-deck-agent-target agent)))
     ((eq (pi-deck-agent-kind agent) 'pi)
      (pilish-close-session buffer))
     (t
      (let ((kill-buffer-query-functions nil))
        (when-let* ((process (get-buffer-process buffer)))
          (set-process-query-on-exit-flag process nil))
        (kill-buffer buffer))))
    (remhash buffer pi-deck--agents)
    (pi-deck--schedule-update)))

(defun pi-deck-stop-agents (agents)
  "Stop AGENTS without asking and remove the panes showing them."
  (when agents
    (with-demoted-errors "pi-deck: %S"
      (pi-deck--remove-views (mapcan #'pi-deck-agent-windows-buffers agents)))
    (mapc #'pi-deck-stop-agent agents)))

(defun pi-deck--with-descendants (agents)
  "Return AGENTS followed by their workers and panels, each once."
  (let ((result (reverse agents)))
    (dolist (agent agents)
      (dolist (child (pi-deck-agent-descendants agent))
        (cl-pushnew child result)))
    (nreverse result)))

(defun pi-deck-stop (agent &optional event)
  "Stop AGENT with its workers and panels.
Asks first when one of them is working or blocked.  EVENT is the initiating
mouse event."
  (let ((stop (pi-deck--with-descendants (list agent))))
    (when (pi-deck-confirm-stop (format "Stopping %s" (pi-deck-agent-name agent)) stop nil event)
      (pi-deck-stop-agents stop))))

;;;; Spaces

(defun pi-deck--drop-space-tab (space frame)
  "Remove SPACE's layout tab from FRAME.
A frame showing SPACE shows its most recent other space instead, else
nothing: its tab leaves the space and shows *scratch*."
  (with-selected-frame frame
    (when-let* ((number (pi-deck--space-tab-number space)))
      (if (and (equal (pi-deck-current-space) space)
               (null (cdr (pi-deck--tabs))))
          (progn
            (pi-deck--set-tab-group nil)
            (pi-deck--single-window)
            (switch-to-buffer (get-scratch-buffer-create) t t))
        (let ((tab-bar-tab-prevent-close-functions nil)
              (tab-bar-close-tab-select 'recent))
          (tab-bar-close-tab number))))))

(defun pi-deck-close-space (space &optional event)
  "Stop SPACE's agents, remove its layouts in all frames and forget it.
Asks first when one of its agents is working or blocked.  EVENT is the
initiating mouse event."
  (interactive (list (pi-deck--read-space "Close space: ") last-nonmenu-event))
  (let ((stop (pi-deck--with-descendants (pi-deck--space-agents space))))
    (when (pi-deck-confirm-stop (format "Closing space %s" space) stop "close" event)
      (pi-deck-stop-agents stop)
      (dolist (frame (pi-deck--client-frames))
        (pi-deck--drop-space-tab space frame))
      (setq pi-deck-spaces
            (cl-remove space pi-deck-spaces :key (lambda (s) (plist-get s :name)) :test #'equal)
            pi-deck--layouts (assoc-delete-all space pi-deck--layouts))
      (when (equal pi-deck--last-space space)
        (setq pi-deck--last-space (pi-deck-current-space)))
      (pi-deck--schedule-update))))

(provide 'pi-deck-close)
;;; pi-deck-close.el ends here
