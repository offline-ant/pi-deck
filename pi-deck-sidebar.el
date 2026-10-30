;;; pi-deck-sidebar.el --- Agent sidebar: the space tree -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The sidebar is a side window on the left of every frame wide enough for
;; it, showing a space tree: each space as a header (name, count that
;; folds it, status, `+' menu) with its agents, workers and panels nested
;; under their parent, each with a ✕ that stops it, folded "N finished workers" with
;; transcript links and a ✕ that clears them, each space's folded "N finished
;; shells" with a ✕ that closes them, and a final "+ New space" row.  Rows of
;; agents the selected frame shows are highlighted.  Its header line holds the
;; status summary (click: next agent needing attention) and the buttons of
;; `pi-deck-sidebar-header-items'.
;;
;; It is mouse-driven: mouse-1 acts (jump, visit a space or show the
;; current space's directory, fold, stop),
;; mouse-2 shows an agent beside the current pane, a double click shows it
;; alone, mouse-3 opens the row's context menu.
;;
;; All frames and spaces share one width, `pi-deck-sidebar-width';
;; dragging the sidebar's edge changes it everywhere and saves it.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-workers)
(require 'pi-deck-close)

(declare-function pi-deck--client-frames "pi-deck-daemon" ())
(declare-function pi-deck-menu "pi-deck-menu" ())
(declare-function pi-deck-answer-dialog "pi-deck-goto" (&optional agent))
(declare-function pi-deck-jump "pi-deck-goto" (agent))
(declare-function pi-deck-next-attention "pi-deck-goto" ())
(declare-function pi-deck-show-only "pi-deck-goto" (agent))
(declare-function pi-deck-popup "pi-deck-menus" (menu &optional event))
(declare-function pi-deck-agent-menu "pi-deck-menus" (agent))
(declare-function pi-deck-space-menu "pi-deck-menus" (space))
(declare-function pi-deck-finished-worker-menu "pi-deck-menus" (record))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

(defcustom pi-deck-sidebar-width 28
  "Width of the agent sidebar in every frame and space.
Dragging the sidebar's edge changes it and saves the choice as a preference."
  :type 'natnum
  :group 'pi-deck)

(defcustom pi-deck-sidebar-min-pane-width 44
  "Columns the sidebar leaves for panes.
Frames without this much room beside the sidebar hide it."
  :type 'natnum
  :group 'pi-deck)

(defcustom pi-deck-sidebar t
  "Whether the agent sidebar is shown.
It hides by itself in frames without `pi-deck-sidebar-min-pane-width'
columns beside it.
`pi-deck-sidebar-toggle' changes it and saves the choice as a preference."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-sidebar-side 'left
  "Frame side of the agent sidebar."
  :type '(choice (const left) (const right))
  :group 'pi-deck)

(defcustom pi-deck-summary-statuses '(blocked done working)
  "Statuses counted in the sidebar's status summary."
  :type '(set (const blocked) (const done) (const working) (const idle) (const unknown))
  :group 'pi-deck)

(defcustom pi-deck-sidebar-header-items nil
  "Functions adding buttons at the right of the sidebar's header line.
Each is called without arguments and returns a string or nil.  A button's
`local-map' text property binds its clicks, e.g. \"<header-line> <mouse-1>\"
to a command reading (interactive \"e\")."
  :type '(repeat function)
  :group 'pi-deck)

(defcustom pi-deck-preferences-file
  (locate-user-emacs-file "var/pi-deck/preferences.eld")
  "File storing preferences changed by commands, such as the sidebar toggle."
  :type 'file
  :group 'pi-deck)

(defconst pi-deck-sidebar-buffer-name "*pi-deck-sidebar*"
  "Name of the sidebar buffer.")

(defvar-local pi-deck-sidebar--collapsed nil
  "Space names whose agents are hidden.")

(defvar-local pi-deck-sidebar--expanded-workers nil
  "Parent agent buffers whose closed workers are listed.")

(defvar-local pi-deck-sidebar--expanded-shells nil
  "Space labels whose finished shells are listed.")

(defvar-local pi-deck-sidebar--current-space nil
  "Space of the selected frame while rendering, underlined.")

(defvar-local pi-deck-sidebar--shown nil
  "Agents the selected frame shows while rendering, highlighted.")

(defface pi-deck-shown-agent '((t :inherit pilish-pi-user-message :extend t))
  "Face for the rows of agents the selected frame shows."
  :group 'pi-deck)

(defface pi-deck-current-space '((t :inherit bold :underline t))
  "Face for the current space's name in the sidebar."
  :group 'pi-deck)

(defface pi-deck-button '((t :inherit shadow))
  "Face for the sidebar's ✕ and + buttons."
  :group 'pi-deck)

(defvar-keymap pi-deck-sidebar-row-map
  :doc "Mouse bindings of sidebar rows."
  "<mouse-1>" #'pi-deck-sidebar-mouse-1
  "<mouse-2>" #'pi-deck-sidebar-mouse-show
  "<double-mouse-1>" #'pi-deck-sidebar-mouse-only
  ;; Menus open on release: a text terminal menu opened on press picks
  ;; its first item at the release.
  "<down-mouse-3>" #'ignore
  "<mouse-3>" #'pi-deck-sidebar-mouse-3)

(defun pi-deck-sidebar--clickable (string help)
  "Return STRING as a clickable row with HELP shown on hover.
Buttons in STRING keep their own help."
  (let ((end (max 0 (1- (length string))))
        (position 0))
    (add-text-properties 0 end
                         (list 'mouse-face 'highlight 'pointer 'hand
                               'keymap pi-deck-sidebar-row-map)
                         string)
    (while (< position end)
      (let ((next (next-single-property-change position 'help-echo string end)))
        (unless (get-text-property position 'help-echo string)
          (put-text-property position next 'help-echo help string))
        (setq position next))))
  string)

(defun pi-deck-sidebar--button (label action help)
  "Return LABEL as a button running ACTION (see `pi-deck-sidebar-mouse-1').
HELP is shown on hover."
  (propertize label 'pi-deck-button action 'face 'pi-deck-button
              'mouse-face 'highlight 'help-echo help 'pointer 'hand))

(defun pi-deck-sidebar--right (button)
  "Return BUTTON aligned to the right edge of the window."
  ;; One column more than BUTTON: text terminals reserve the last one.
  (concat (propertize " " 'display `(space :align-to (- right ,(+ 2 (string-width button)))))
          button))

;;;; Rendering

(defun pi-deck-sidebar--tree (agents)
  "Return AGENTS as (AGENT . DEPTH) in parent-child order."
  (let (result)
    (cl-labels ((walk (agent depth seen)
                  (push (cons agent depth) result)
                  (dolist (child agents)
                    (when (and (eq (pi-deck-agent-parent child) (pi-deck-agent-buffer agent))
                               (not (memq child seen)))
                      (walk child (1+ depth) (cons child seen))))))
      (dolist (agent agents)
        (unless (cl-find (pi-deck-agent-parent agent) agents :key #'pi-deck-agent-buffer)
          (walk agent 0 (list agent)))))
    (nreverse result)))

(defun pi-deck-sidebar--agent-line (agent depth)
  "Return the row for AGENT at tree DEPTH."
  (let* ((detail (pi-deck-agent-detail agent))
         (indent (make-string (* 2 depth) ?\s))
         (line (concat " " indent (pi-deck-status-icon (pi-deck-agent-status agent)) " "
                       (truncate-string-to-width
                        (concat (pi-deck-agent-name agent)
                                (when detail (propertize (concat "  " detail) 'face 'shadow)))
                        (max 4 (- pi-deck-sidebar-width (length indent) 7)) nil nil "…")
                       (pi-deck-sidebar--right
                        (pi-deck-sidebar--button "✕" 'stop "mouse-1: stop (asks when busy)"))
                       "\n")))
    (when (memq agent pi-deck-sidebar--shown)
      (add-face-text-property 0 (length line) 'pi-deck-shown-agent t line))
    (pi-deck-sidebar--clickable
     (propertize line 'pi-deck-agent agent)
     "mouse-1: jump to this agent; double-click: show only it; mouse-2: show it beside; mouse-3: menu")))

(defun pi-deck-sidebar--finished-row (indent expanded label button)
  "Return a foldable row of finished things reading LABEL, ending in BUTTON.
INDENT is its leading space and EXPANDED says whether its list shows."
  (concat indent (if expanded "▾ " "▸ ")
          (propertize (truncate-string-to-width
                       label (max 4 (- pi-deck-sidebar-width (length indent) 6)) nil nil "…")
                      'face 'shadow)
          (pi-deck-sidebar--right button)
          "\n"))

(defun pi-deck-sidebar--insert-finished (agent depth)
  "Insert AGENT's closed workers at tree DEPTH: a count, or the list."
  (when-let* ((buffer (pi-deck-agent-buffer agent))
              (records (pi-deck-finished-workers buffer)))
    (let ((indent (concat " " (make-string (* 2 depth) ?\s)))
          (expanded (memq buffer pi-deck-sidebar--expanded-workers)))
      (insert (pi-deck-sidebar--clickable
               (propertize
                (pi-deck-sidebar--finished-row
                 indent expanded
                 (format "%d finished worker%s" (length records) (if (cdr records) "s" ""))
                 (pi-deck-sidebar--button
                  "✕" 'clear-workers "mouse-1: clear this list; session files stay on disk"))
                'pi-deck-finished buffer)
               "mouse-1: show or hide closed workers"))
      (when expanded
        (dolist (record records)
          (insert (pi-deck-sidebar--clickable
                   (propertize
                    (concat indent "  ✓ " (plist-get record :name)
                            (propertize (format "  %s" (pi-deck-age (plist-get record :time)))
                                        'face 'shadow)
                            "\n")
                    'pi-deck-worker-record record)
                   "mouse-1: read this worker's session; mouse-3: menu")))))))

(defun pi-deck-sidebar--insert-finished-shells (label agents)
  "Insert the ended terminals AGENTS of the space LABEL: a count, or the list."
  (when agents
    (let ((expanded (member label pi-deck-sidebar--expanded-shells)))
      (insert (pi-deck-sidebar--clickable
               (propertize
                (pi-deck-sidebar--finished-row
                 " " expanded
                 (format "%d finished shell%s" (length agents) (if (cdr agents) "s" ""))
                 (pi-deck-sidebar--button
                  "✕" 'close-shells "mouse-1: close these shells; their output goes"))
                'pi-deck-finished-shells (cons label agents))
               "mouse-1: show or hide finished shells"))
      (when expanded
        (dolist (agent agents)
          (insert (pi-deck-sidebar--agent-line agent 1)))))))

(defun pi-deck-sidebar--space-header (space status count)
  "Return the header row for SPACE with rolled-up STATUS and agent COUNT.
Its name visits the space, its count folds it and its `+' opens the
space's menu."
  (let ((name (or space "other")))
    (pi-deck-sidebar--clickable
     (propertize
      (concat (propertize name 'face (if (and space (equal space pi-deck-sidebar--current-space))
                                         'pi-deck-current-space
                                       'bold))
              " "
              (pi-deck-sidebar--button (format "(%d)" count) 'fold "mouse-1: fold or unfold")
              " "
              (pi-deck-status-icon (or status 'idle))
              (when space
                (pi-deck-sidebar--right
                 (pi-deck-sidebar--button "+" 'space-menu "mouse-1: new agent or shell")))
              "\n")
      'pi-deck-space name)
     "mouse-1: switch to this space; mouse-3: menu")))

(defun pi-deck-sidebar--insert-spaces (agents tab-index)
  "Insert AGENTS by home space (TAB-INDEX caches `pi-deck-tab-index').
Each space lists its agents in their stable creation order, workers and
panels under their parent, and folds its finished shells into one row;
agents without a space come last, under \"other\"."
  (dolist (space (append (pi-deck-space-names) (list nil)))
    (let ((members (cl-remove-if-not
                    (lambda (agent) (equal (pi-deck-agent-space agent tab-index) space))
                    agents)))
      (when (or space members)
        (insert (pi-deck-sidebar--space-header
                 space (pi-deck-rollup (mapcar #'pi-deck-agent-status members))
                 (length members)))
        (unless (member (or space "other") pi-deck-sidebar--collapsed)
          (dolist (item (pi-deck-sidebar--tree
                         (seq-remove #'pi-deck-agent-finished-shell-p members)))
            (insert (pi-deck-sidebar--agent-line (car item) (cdr item)))
            (pi-deck-sidebar--insert-finished (car item) (1+ (cdr item))))
          (pi-deck-sidebar--insert-finished-shells
           (or space "other") (seq-filter #'pi-deck-agent-finished-shell-p members)))))))

(defun pi-deck-summary-string (&optional summary short)
  "Return a status summary like \"◉ 2 blocked · ● 1 done\" for SUMMARY.
SHORT leaves out the status names: \"◉ 2 · ● 1\"."
  (mapconcat (lambda (cell)
               (concat (pi-deck-status-icon (car cell))
                       (propertize (if short (format " %d" (cdr cell))
                                     (format " %d %s" (cdr cell) (car cell)))
                                   'face (pi-deck-status-face (car cell)))))
             (or summary (pi-deck-summary))
             (propertize " · " 'face 'shadow)))

(defvar-keymap pi-deck-sidebar--summary-map
  :doc "Header line bindings of the sidebar's status summary."
  "<header-line> <down-mouse-1>" #'ignore
  "<header-line> <mouse-1>" #'pi-deck-next-attention)

(defun pi-deck-sidebar-header ()
  "Return the sidebar's header line: status summary and extra buttons.
The summary leaves out status names when they would not fit."
  (let* ((summary (cl-remove-if-not (lambda (cell) (memq (car cell) pi-deck-summary-statuses))
                                    (pi-deck-summary)))
         (items (mapconcat #'identity
                           (delq nil (mapcar #'funcall pi-deck-sidebar-header-items))
                           " "))
         (room (- (window-total-width) (string-width items) 3))
         (status (cond ((null summary)
                        (propertize (if (pi-deck-agents) "all idle" "no agents") 'face 'shadow))
                       ((<= (string-width (pi-deck-summary-string summary)) room)
                        (pi-deck-summary-string summary))
                       (t (pi-deck-summary-string summary t)))))
    (string-replace
     "%" "%%"
     (concat " "
             (propertize status 'mouse-face 'mode-line-highlight 'pointer 'hand
                         'help-echo "mouse-1: next agent needing attention"
                         'local-map pi-deck-sidebar--summary-map)
             (unless (string-empty-p items)
               (concat (propertize " " 'display `(space :align-to (- right ,(1+ (string-width items)))))
                       items))))))

(defun pi-deck-sidebar--render ()
  "Rebuild the current sidebar buffer, keeping point on its row."
  (let* ((inhibit-read-only t)
         (agent (get-text-property (point) 'pi-deck-agent))
         (space (get-text-property (point) 'pi-deck-space))
         (line (line-number-at-pos))
         (window-start (and (get-buffer-window) (window-start (get-buffer-window)))))
    (setq pi-deck-sidebar--current-space (pi-deck-current-space)
          pi-deck-sidebar--shown
          (pi-deck--agents-shown-by (mapcar #'window-buffer (window-list nil 'no-minibuf))))
    (erase-buffer)
    (pi-deck-sidebar--insert-spaces (pi-deck-agents) (pi-deck-tab-index))
    (insert "\n" (pi-deck-sidebar--clickable
                  (propertize (concat (propertize "+ New space" 'face 'link) "\n")
                              'pi-deck-action #'pi-deck-create-space)
                  "mouse-1: create a space for a project directory"))
    (goto-char (point-min))
    (let ((match (or (and agent (text-property-any (point-min) (point-max) 'pi-deck-agent agent))
                     (and space (text-property-any (point-min) (point-max) 'pi-deck-space space)))))
      (if match (goto-char match) (forward-line (1- line))))
    (when window-start
      (set-window-start (get-buffer-window) (min window-start (point-max)) t))))

(defun pi-deck-sidebar-refresh (&optional buffer)
  "Rebuild BUFFER (default current) if it is the sidebar."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'pi-deck-sidebar-mode)
      (pi-deck-sidebar--render))))

(defun pi-deck-sidebar-refresh-visible ()
  "Rebuild the sidebar when it is visible."
  (when-let* ((buffer (get-buffer pi-deck-sidebar-buffer-name)))
    (when (get-buffer-window buffer 'visible)
      (pi-deck-sidebar-refresh buffer))))

;;;; Commands

(defun pi-deck-sidebar--agent-at-point ()
  "Return the agent on this line or signal a user error."
  (or (get-text-property (line-beginning-position) 'pi-deck-agent)
      (user-error "No agent on this line")))

(defun pi-deck-sidebar-visit-space (space)
  "Unfold SPACE and show its layout.
The unfolded current space shows its directory beside the current pane."
  (let ((folded (member space pi-deck-sidebar--collapsed)))
    (setq pi-deck-sidebar--collapsed (delete space pi-deck-sidebar--collapsed))
    (cond ((not (pi-deck-space space)) (pi-deck-sidebar--render))
          ((not (equal space (pi-deck-current-space))) (pi-deck-select-space space))
          (folded (pi-deck-sidebar--render))
          (t (pi-deck-show-space-directory space)))))

(defun pi-deck-sidebar-jump ()
  "Act on this line: run its action, visit its space, open its closed
worker, show its closed workers or finished shells, or jump to its agent."
  (interactive)
  (let ((position (line-beginning-position)))
    (cond
     ((get-text-property position 'pi-deck-action)
      (call-interactively (get-text-property position 'pi-deck-action)))
     ((get-text-property position 'pi-deck-worker-record)
      (pi-deck-open-finished-worker (get-text-property position 'pi-deck-worker-record)))
     ((get-text-property position 'pi-deck-space)
      (pi-deck-sidebar-visit-space (get-text-property position 'pi-deck-space)))
     ((or (get-text-property position 'pi-deck-finished)
          (get-text-property position 'pi-deck-finished-shells))
      (pi-deck-sidebar-toggle-fold))
     (t (pi-deck-jump (pi-deck-sidebar--agent-at-point))))))

(defun pi-deck-sidebar--clear-workers (parent)
  "Clear the closed workers listed under PARENT's buffer."
  (setq pi-deck-sidebar--expanded-workers (delq parent pi-deck-sidebar--expanded-workers))
  (pi-deck-clear-finished-workers parent)
  (pi-deck-sidebar--render))

(defun pi-deck-sidebar--close-shells (label agents)
  "Close the finished shells AGENTS listed under the space LABEL."
  (setq pi-deck-sidebar--expanded-shells (delete label pi-deck-sidebar--expanded-shells))
  (pi-deck-stop-agents agents)
  (pi-deck-sidebar--render))

(defun pi-deck-sidebar-mouse-1 (event)
  "Act on what EVENT clicked: a ✕, + or fold button, else the line.
Buttons leave the selected window alone; lines act like \\[pi-deck-sidebar-jump]."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (position (posn-point posn)))
    (with-current-buffer (window-buffer window)
      (let ((row (save-excursion (goto-char position) (line-beginning-position))))
        (pcase (get-text-property position 'pi-deck-button)
          ('fold (with-selected-window window
                   (goto-char position)
                   (pi-deck-sidebar-toggle-fold)))
          ('stop (pi-deck-stop (get-text-property row 'pi-deck-agent) event))
          ('clear-workers (pi-deck-sidebar--clear-workers
                           (get-text-property row 'pi-deck-finished)))
          ('close-shells (let ((shells (get-text-property row 'pi-deck-finished-shells)))
                           (pi-deck-sidebar--close-shells (car shells) (cdr shells))))
          ('space-menu (pi-deck-popup (pi-deck-space-menu (get-text-property row 'pi-deck-space))
                                      event))
          (_ (mouse-set-point event)
             (pi-deck-sidebar-jump)))))))

(defun pi-deck-sidebar-mouse-show (event)
  "Show the agent clicked in EVENT beside the current pane."
  (interactive "e")
  (let ((posn (event-start event)))
    (when-let* ((agent (with-current-buffer (window-buffer (posn-window posn))
                         (get-text-property (posn-point posn) 'pi-deck-agent))))
      (pi-deck-show-beside agent))))

(defun pi-deck-sidebar-mouse-only (event)
  "Show the agent double-clicked in EVENT as the only pane of its layout.
The first click already jumped to it; buttons ignore double clicks."
  (interactive "e")
  (let* ((posn (event-start event))
         (agent (with-current-buffer (window-buffer (posn-window posn))
                  (and (not (get-text-property (posn-point posn) 'pi-deck-button))
                       (get-text-property (posn-point posn) 'pi-deck-agent)))))
    (when agent (pi-deck-show-only agent))))

(defun pi-deck-sidebar-mouse-3 (event)
  "Open the context menu of the line clicked in EVENT."
  (interactive "e")
  (let* ((posn (event-start event))
         (position (posn-point posn))
         (menu (with-current-buffer (window-buffer (posn-window posn))
                 (let ((agent (get-text-property position 'pi-deck-agent))
                       (space (get-text-property position 'pi-deck-space))
                       (record (get-text-property position 'pi-deck-worker-record)))
                   (cond (agent (pi-deck-agent-menu agent))
                         ((and space (pi-deck-space space)) (pi-deck-space-menu space))
                         (record (pi-deck-finished-worker-menu record)))))))
    (when menu (pi-deck-popup menu event))))

(defun pi-deck-sidebar-toggle-fold ()
  "Fold the space at point, or show or hide the finished things at point."
  (interactive)
  (let* ((row (line-beginning-position))
         (parent (get-text-property row 'pi-deck-finished))
         (shells (get-text-property row 'pi-deck-finished-shells)))
    (cond
     (parent
      (setq pi-deck-sidebar--expanded-workers
            (if (memq parent pi-deck-sidebar--expanded-workers)
                (delq parent pi-deck-sidebar--expanded-workers)
              (cons parent pi-deck-sidebar--expanded-workers))))
     (shells
      (setq pi-deck-sidebar--expanded-shells
            (if (member (car shells) pi-deck-sidebar--expanded-shells)
                (delete (car shells) pi-deck-sidebar--expanded-shells)
              (cons (car shells) pi-deck-sidebar--expanded-shells))))
     (t
      (let ((space (or (get-text-property row 'pi-deck-space)
                       (user-error "No space on this line"))))
        (setq pi-deck-sidebar--collapsed
              (if (member space pi-deck-sidebar--collapsed)
                  (delete space pi-deck-sidebar--collapsed)
                (cons space pi-deck-sidebar--collapsed)))))))
  (pi-deck-sidebar--render))

(defun pi-deck-sidebar-show ()
  "Show the agent at point beside the current pane, keeping the sidebar selected."
  (interactive)
  (save-selected-window
    (pi-deck-show-beside (pi-deck-sidebar--agent-at-point))))

(defun pi-deck-sidebar-next (n)
  "Move to the Nth next agent line."
  (interactive "p")
  (dotimes (_ (abs n))
    (let ((step (if (> n 0) 1 -1)))
      (forward-line step)
      (while (and (not (get-text-property (point) 'pi-deck-agent))
                  (zerop (forward-line step))
                  (not (if (> step 0) (eobp) (bobp)))))
      (unless (get-text-property (point) 'pi-deck-agent)
        (goto-char (or (if (> step 0)
                           (previous-single-property-change (point-max) 'pi-deck-agent)
                         (text-property-not-all (point-min) (point-max) 'pi-deck-agent nil))
                       (point)))
        (beginning-of-line)))))

(defun pi-deck-sidebar-previous (n)
  "Move to the Nth previous agent line."
  (interactive "p")
  (pi-deck-sidebar-next (- n)))

(defun pi-deck-sidebar-stop ()
  "Stop the agent at point; asks first when it is working or blocked."
  (interactive)
  (pi-deck-stop (pi-deck-sidebar--agent-at-point))
  (pi-deck-sidebar--render))

(defun pi-deck-sidebar-answer ()
  "Answer the dialog of the agent at point."
  (interactive)
  (pi-deck-answer-dialog (pi-deck-sidebar--agent-at-point)))

(defun pi-deck-sidebar--space-at-point ()
  "Return the registered space of this line (header or agent), or nil."
  (let ((space (or (get-text-property (line-beginning-position) 'pi-deck-space)
                   (when-let* ((agent (get-text-property (line-beginning-position)
                                                         'pi-deck-agent)))
                     (pi-deck-agent-space agent)))))
    (and (pi-deck-space space) space)))

(defun pi-deck-sidebar-new-agent ()
  "Start a Pi agent in the space at point (else a space read), and show it."
  (interactive)
  (when-let* ((space (pi-deck-sidebar--space-at-point)))
    (pi-deck-select-space space))
  (call-interactively #'pi-deck-new-agent))

(defun pi-deck-sidebar-new-shell ()
  "Open a shell in the space at point (else a space read), and show it."
  (interactive)
  (when-let* ((space (pi-deck-sidebar--space-at-point)))
    (pi-deck-select-space space))
  (call-interactively #'pi-deck-new-shell))

(defvar-keymap pi-deck-sidebar-mode-map
  :doc "Keymap for the pi-deck sidebar."
  :parent special-mode-map
  "RET" #'pi-deck-sidebar-jump
  "o" #'pi-deck-sidebar-show
  "TAB" #'pi-deck-sidebar-toggle-fold
  "g" #'pi-deck-sidebar-refresh
  "n" #'pi-deck-sidebar-next
  "p" #'pi-deck-sidebar-previous
  "j" #'pi-deck-sidebar-next
  "k" #'pi-deck-sidebar-previous
  "A" #'pi-deck-sidebar-answer
  "K" #'pi-deck-sidebar-stop
  "+" #'pi-deck-create-space
  "N" #'pi-deck-sidebar-new-agent
  "E" #'pi-deck-sidebar-new-shell
  "m" #'pi-deck-menu
  "?" #'pi-deck-menu
  "q" #'pi-deck-sidebar-toggle)

(define-derived-mode pi-deck-sidebar-mode special-mode "Pi-Deck"
  "Sidebar listing spaces and their Pi agents and terminals.
\\{pi-deck-sidebar-mode-map}"
  (setq-local truncate-lines t
              cursor-type nil
              header-line-format '(:eval (pi-deck-sidebar-header)))
  (hl-line-mode 1))

;; Evil users stay in normal state, so leader keys, `gg' and `/' keep
;; working.  The sidebar's own keys go into its normal-state keymap, which
;; takes precedence over Evil's and evil-collection's `special-mode' ones;
;; refresh moves from `g' to `gr' to leave the `g' prefix intact.
(defun pi-deck-sidebar--evil-setup ()
  "Set up Evil normal-state bindings for the sidebar."
  (evil-set-initial-state 'pi-deck-sidebar-mode 'normal)
  (let (bindings)
    (map-keymap-internal
     (lambda (event command)
       (unless (eq event ?g)
         (setq bindings (append bindings (list (vector event) command)))))
     pi-deck-sidebar-mode-map)
    (apply #'evil-define-key* 'normal pi-deck-sidebar-mode-map
           "gr" #'pi-deck-sidebar-refresh bindings)))

(with-eval-after-load 'evil (pi-deck-sidebar--evil-setup))

(defun pi-deck-sidebar-buffer ()
  "Return the sidebar buffer, creating it when needed."
  (or (get-buffer pi-deck-sidebar-buffer-name)
      (with-current-buffer (get-buffer-create pi-deck-sidebar-buffer-name)
        (pi-deck-sidebar-mode)
        (pi-deck-sidebar--render)
        (current-buffer))))

;;;; Window

(defun pi-deck--sidebar-fits-p (&optional frame)
  "Return non-nil when FRAME has room for the sidebar and panes beside it."
  (>= (frame-width frame) (+ pi-deck-sidebar-width pi-deck-sidebar-min-pane-width)))

(defun pi-deck--sidebar-show ()
  "Show the sidebar in the selected frame."
  (let ((window (display-buffer-in-side-window
                 (pi-deck-sidebar-buffer)
                 `((side . ,pi-deck-sidebar-side) (slot . 0)
                   (window-width . ,pi-deck-sidebar-width)
                   (preserve-size . (t . nil))
                   (window-parameters . ((no-delete-other-windows . t)))))))
    (set-window-dedicated-p window t)
    (pi-deck-sidebar-refresh (window-buffer window))
    window))

(defun pi-deck--sidebar-set-width (window)
  "Give the sidebar WINDOW the width `pi-deck-sidebar-width'."
  (let ((delta (- pi-deck-sidebar-width (window-total-width window))))
    (unless (zerop delta)
      (window-preserve-size window t nil)
      (ignore-errors (window-resize window delta t))
      (window-preserve-size window t t))))

(defun pi-deck--sidebar-sync (&rest _)
  "Show, hide or size the selected frame's sidebar as the settings say.
It shows when `pi-deck-sidebar' is on and the frame fits it."
  (let ((window (get-buffer-window pi-deck-sidebar-buffer-name))
        (wanted (and pi-deck-sidebar (pi-deck--sidebar-fits-p))))
    (cond ((and wanted (not window)) (pi-deck--sidebar-show))
          ((and window (not wanted)) (delete-window window))
          (window (pi-deck--sidebar-set-width window)))))

(defun pi-deck--on-window-size-change (frame)
  "Follow size changes in FRAME, for `window-size-change-functions'.
A sidebar resized within an unchanged frame was dragged: its width becomes
the saved `pi-deck-sidebar-width'.  A resized frame shows or hides the
sidebar."
  (let ((window (get-buffer-window pi-deck-sidebar-buffer-name frame))
        (root (frame-root-window frame)))
    (if (/= (window-old-pixel-width root) (window-pixel-width root))
        (run-at-time 0 nil (lambda ()
                             (when (frame-live-p frame)
                               (with-selected-frame frame (pi-deck--sidebar-sync)))))
      (when (and window
                 (> (window-old-pixel-width window) 0)
                 (/= (window-old-pixel-width window) (window-pixel-width window))
                 (/= (window-total-width window) pi-deck-sidebar-width))
        (setq pi-deck-sidebar-width (window-total-width window))
        (pi-deck-sidebar-refresh (window-buffer window))
        (with-demoted-errors "pi-deck: %S"
          (pi-deck-save-preference 'pi-deck-sidebar-width))))))

;;;; Preferences

(defconst pi-deck-preference-variables '(pi-deck-sidebar pi-deck-sidebar-width)
  "Variables commands save to `pi-deck-preferences-file'.")

(defun pi-deck--read-preferences ()
  "Return the saved preferences alist, or nil."
  (when (file-readable-p pi-deck-preferences-file)
    (with-temp-buffer
      (insert-file-contents pi-deck-preferences-file)
      (read (current-buffer)))))

(defun pi-deck-save-preference (variable)
  "Save VARIABLE's current value, keeping the other saved preferences."
  (let ((preferences (pi-deck--read-preferences)))
    (setf (alist-get variable preferences) (symbol-value variable))
    (make-directory (file-name-directory pi-deck-preferences-file) t)
    (with-file-modes #o600
      (with-temp-file pi-deck-preferences-file
        (let ((print-length nil) (print-level nil))
          (insert ";; pi-deck preferences -*- lisp-data -*-\n")
          (prin1 preferences (current-buffer))
          (insert "\n"))))))

(defun pi-deck-load-preferences ()
  "Load the preferences saved by commands, if any."
  (dolist (cell (pi-deck--read-preferences))
    (when (memq (car cell) pi-deck-preference-variables)
      (set (car cell) (cdr cell)))))

;;;###autoload
(defun pi-deck-sidebar-toggle ()
  "Toggle the agent sidebar in all frames, remembering the choice."
  (interactive)
  (setq pi-deck-sidebar (not pi-deck-sidebar))
  (dolist (frame (pi-deck--client-frames))
    (with-selected-frame frame (pi-deck--sidebar-sync)))
  (with-demoted-errors "pi-deck: %S" (pi-deck-save-preference 'pi-deck-sidebar)))

(provide 'pi-deck-sidebar)
;;; pi-deck-sidebar.el ends here
