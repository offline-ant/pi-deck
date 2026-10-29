;;; pi-deck-close.el --- Closing tabs, panes and spaces stops what they show -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Closing follows Herdr: closing a tab, a pane or a space stops the agents
;; and panels it shows, unless something else still shows them.  "Else" is
;; another tab of the same frame (for a pane also another window of its
;; tab).  Workers and panels of a stopped agent stop with it unless they have
;; a view of their own elsewhere.  Stopping an agent from the sidebar or
;; overview (its ✕) stops it wherever it is shown.
;;
;; Closing asks first only when something it would stop is working or
;; blocked, naming what will stop.  Commands started by the mouse ask with
;; a popup dialog, so the mouse alone can answer; keyboard commands ask in
;; the minibuffer.
;;
;; After stopping, windows showing the stopped agents are deleted and tabs
;; that showed nothing else close, in every client frame.

;;; Code:

(require 'tab-bar)
(require 'pi-deck-core)
(require 'pi-deck-spaces)

(declare-function pi-deck--client-frames "pi-deck-daemon" ())
(declare-function pi-deck-root-ensure "pi-deck-overview" (&optional frame))
(declare-function pi-deck-root-show "pi-deck-overview" ())
(declare-function pi-orchestration-stop-target "pi-orchestration" (id))

(defvar pi-deck-sidebar-buffer-name)
(defvar pi-deck-overview-buffer-name)

(defvar pi-deck--closing-internally nil
  "Non-nil while pi-deck closes tabs itself; `pi-deck--tab-close-guard' then
lets every close through without asking or stopping anything.")

(defvar pi-deck-ask-function #'pi-deck--ask
  "Function asking a yes/no QUESTION with a CHOICE label and the initiating EVENT.
It returns non-nil to go ahead.")

;;;; Views

(defun pi-deck--view-buffer-p (buffer)
  "Return non-nil when BUFFER is content, not the sidebar or overview."
  (and (buffer-live-p buffer)
       (not (member (buffer-name buffer)
                    (list pi-deck-sidebar-buffer-name pi-deck-overview-buffer-name)))))

(defun pi-deck--shown-by-p (agent buffers)
  "Return non-nil when one of BUFFERS shows AGENT."
  (cl-some (lambda (buffer) (memq buffer buffers)) (pi-deck-agent-windows-buffers agent)))

(defun pi-deck--agents-shown-by (buffers)
  "Return the agents shown by BUFFERS."
  (cl-remove-if-not (lambda (agent) (pi-deck--shown-by-p agent buffers)) (pi-deck-agents)))

(defun pi-deck--other-tab-buffers (tab &optional frame)
  "Return the buffers shown by FRAME's tabs other than TAB."
  (let (buffers)
    (dolist (other (pi-deck--tabs frame))
      (unless (eq other tab)
        (setq buffers (append (pi-deck--tab-buffers other frame) buffers))))
    buffers))

(defun pi-deck--stop-set (agents others &optional descendant-others)
  "Return AGENTS not shown by OTHERS, plus their descendants.
Descendants (workers and panels) not already in AGENTS are included unless
DESCENDANT-OTHERS (default OTHERS) shows them."
  (let ((descendant-others (or descendant-others others))
        result)
    (dolist (agent agents)
      (unless (pi-deck--shown-by-p agent others)
        (cl-pushnew agent result)
        (dolist (child (pi-deck-agent-descendants agent))
          (unless (or (memq child agents) (pi-deck--shown-by-p child descendant-others))
            (cl-pushnew child result)))))
    (nreverse result)))

(defun pi-deck-tab-close-plan (tab &optional frame)
  "Return the agents closing TAB of FRAME would stop."
  (pi-deck--stop-set (pi-deck--agents-shown-by (pi-deck--tab-buffers tab frame))
                     (pi-deck--other-tab-buffers tab frame)))

(defun pi-deck--pane-windows (window)
  "Return the live windows of WINDOW's pane: a Pilish pair, else WINDOW."
  (if-let* ((pair (pilish-window-pair window)))
      (list (car pair) (cdr pair))
    (list window)))

(defun pi-deck-pane-close-plan (window)
  "Return the agents closing WINDOW's pane would stop."
  (let* ((pane (pi-deck--pane-windows window))
         (agents (pi-deck--agents-shown-by (mapcar #'window-buffer pane)))
         (others (append
                  (delq nil (mapcar (lambda (other)
                                      (unless (or (memq other pane)
                                                  (window-parameter other 'window-side))
                                        (window-buffer other)))
                                    (window-list (window-frame window) 'no-minibuf)))
                  (pi-deck--other-tab-buffers (tab-bar--current-tab-find nil (window-frame window))
                                              (window-frame window)))))
    (pi-deck--stop-set agents others)))

(defun pi-deck-agent-stop-plan (agent)
  "Return the agents stopping AGENT stops: it and its descendants without views."
  (let ((all (pi-deck--other-tab-buffers nil)))
    (pi-deck--stop-set (list agent) nil all)))

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

(defun pi-deck--recent-tab-number (space &optional exclude frame)
  "Return the 1-based number of SPACE's most recent tab in FRAME but EXCLUDE."
  (let ((best nil) (best-time -1) (index 0))
    (dolist (tab (pi-deck--tabs frame))
      (cl-incf index)
      (when (and (equal (pi-deck--tab-group tab) space) (not (eq tab exclude)))
        (let ((time (or (alist-get 'time tab) 0)))
          (when (> time best-time) (setq best index best-time time)))))
    best))

(defun pi-deck--tab-after-close (tab)
  "Return the tab number to select after closing the current TAB.
That is the space's most recent other tab, else the root tab."
  (or (pi-deck--recent-tab-number (pi-deck--tab-group tab) tab)
      (progn (pi-deck-root-ensure)
             (1+ (cl-position-if #'pi-deck-root-tab-p (pi-deck--tabs))))))

(defun pi-deck--close-tabs-internally (numbers)
  "Close the tabs with 1-based NUMBERS in the selected frame, without asking."
  (let ((pi-deck--closing-internally t))
    (dolist (number (sort (copy-sequence numbers) #'>))
      (let* ((tabs (pi-deck--tabs))
             (tab (nth (1- number) tabs)))
        (tab-bar-close-tab number (and (eq (car tab) 'current-tab)
                                       (pi-deck--tab-after-close tab)))))))

(defun pi-deck--remove-views (buffers)
  "Delete the views of BUFFERS in every client frame.
Tabs (but the root) showing nothing besides BUFFERS close; elsewhere windows
showing them are deleted, a Pilish pair as a whole."
  (dolist (frame (pi-deck--client-frames))
    (with-selected-frame frame
      (let ((index 0) close)
        (dolist (tab (pi-deck--tabs))
          (cl-incf index)
          (let ((shown (cl-remove-if-not #'pi-deck--view-buffer-p
                                         (pi-deck--tab-buffers tab frame))))
            (when (and shown (not (pi-deck-root-tab-p tab))
                       (cl-every (lambda (buffer) (memq buffer buffers)) shown))
              (push index close))))
        (pi-deck--close-tabs-internally close))
      (dolist (window (window-list frame 'no-minibuf))
        (when (and (window-live-p window)
                   (memq (window-buffer window) buffers)
                   (not (window-parameter window 'window-side))
                   (not (eq (pi-deck--pane-of window) (window-main-window frame))))
          (delete-window (pi-deck--pane-of window)))))))

(defun pi-deck-stop-agents (agents)
  "Stop AGENTS without asking and remove the views showing only them."
  (when agents
    (with-demoted-errors "pi-deck: %S"
      (pi-deck--remove-views (mapcan #'pi-deck-agent-windows-buffers agents)))
    (mapc #'pi-deck-stop-agent agents)))

;;;; Tabs

(defun pi-deck--tab-close-guard (tab _last-tab-p)
  "Decide whether TAB may close (for `tab-bar-tab-prevent-close-functions').
The root tab is pinned.  Other tabs close Herdr-style: what only they show
stops, after asking when that is working or blocked.  Return non-nil to
prevent closing."
  (cond
   ((or pi-deck--closing-internally (not (bound-and-true-p pi-deck-mode))) nil)
   ((pi-deck-root-tab-p tab) t)
   (t (let ((stop (pi-deck-tab-close-plan tab)))
        (if (pi-deck-confirm-stop (format "Closing tab %s" (alist-get 'name tab))
                                  stop "close")
            (progn
              ;; After the tab closed: stopping removes views, which may
              ;; close other tabs and shift tab numbers.
              (when stop (run-at-time 0 nil #'pi-deck-stop-agents stop))
              nil)
          t)))))

(defun pi-deck-close-tab (&optional tab-number)
  "Close tab TAB-NUMBER (default the current tab) and stop what only it shows.
Asks first when that is working or blocked.  Closing the current tab selects
the space's most recent other tab, else the overview.  The root tab is
pinned."
  (interactive)
  (let* ((tabs (pi-deck--tabs))
         (index (if tab-number (1- tab-number) (tab-bar--current-tab-index tabs)))
         (tab (nth index tabs)))
    (when (pi-deck-root-tab-p tab) (user-error "The overview tab is pinned"))
    (tab-bar-close-tab (1+ index)
                       (and (eq (car tab) 'current-tab) (pi-deck--tab-after-close tab)))))

;;;; Panes and agents

(defun pi-deck-close-pane (&optional window event)
  "Close WINDOW's pane and stop its agent unless something else shows it.
WINDOW defaults to the selected pane; a chat closes with its input.  The
tab's only pane closes the tab.  EVENT is the initiating mouse event."
  (interactive (list nil last-nonmenu-event))
  (let* ((window (or window (pi-deck--pane-window)))
         (pane (pi-deck--pane-of window)))
    (if (eq pane (window-main-window (window-frame window)))
        (with-selected-frame (window-frame window) (pi-deck-close-tab))
      (let ((stop (pi-deck-pane-close-plan window)))
        (when (pi-deck-confirm-stop "Closing this pane" stop "close" event)
          (delete-window pane)
          (pi-deck-stop-agents stop))))))

(defun pi-deck-stop (agent &optional event)
  "Stop AGENT wherever it is shown, with its workers and panels without views.
Asks first when one of them is working or blocked.  EVENT is the initiating
mouse event."
  (let ((stop (pi-deck-agent-stop-plan agent)))
    (when (pi-deck-confirm-stop (format "Stopping %s" (pi-deck-agent-name agent)) stop nil event)
      (pi-deck-stop-agents stop))))

;;;; Spaces

(defun pi-deck-close-space (space &optional event)
  "Close SPACE's tabs in all frames, stop its agents and forget it.
Asks first when one of its agents is working or blocked.  EVENT is the
initiating mouse event."
  (interactive (list (pi-deck--read-space "Close space: " t) last-nonmenu-event))
  (when (equal space pi-deck-root-name)
    (user-error "The overview cannot be closed"))
  (let* ((agents (pi-deck--space-agents space))
         (stop (pi-deck--stop-set agents nil (pi-deck--other-tab-buffers nil))))
    (when (pi-deck-confirm-stop (format "Closing space %s" space) stop "close" event)
      (dolist (frame (pi-deck--client-frames))
        (with-selected-frame frame
          (pi-deck--close-tabs-internally (mapcar #'cadr (pi-deck--space-tab-entries space)))))
      (setq pi-deck-spaces
            (cl-remove space pi-deck-spaces :key (lambda (s) (plist-get s :name)) :test #'equal))
      (pi-deck-stop-agents stop)
      (pi-deck--schedule-update))))

(provide 'pi-deck-close)
;;; pi-deck-close.el ends here
