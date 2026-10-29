;;; pi-deck-overview.el --- Root overview tab and agent sidebar -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The overview (`*pi-deck*', shown by the tab bar's ⌂ in the hidden root
;; tab) lists daemon status, then every agent grouped by space and tab
;; (workers nested under their parent), with status filters and a priority
;; sort.  It ends with the key sheet (`pi-deck-keys').
;;
;; The sidebar (a side window on the left of every other tab) is a space
;; tree: ⌂ Overview, then each space as a header (status, ▸/▾ fold, name,
;; `+' menu) with its agents, workers and panels nested under their parent,
;; each with a ✕ that stops it, and a final "+ New space" row.
;;
;; Both views are mouse-driven: mouse-1 acts (jump, visit a space, fold,
;; stop), mouse-2 shows an agent beside the current pane, mouse-3 opens the
;; row's context menu.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-workers)
(require 'pi-deck-close)
(require 'pi-deck-keys)

(declare-function pi-deck-save-preference "pi-deck-tabbar" (variable))
(declare-function pi-deck-daemon-info-lines "pi-deck-daemon" ())
(declare-function pi-deck--client-frames "pi-deck-daemon" ())
(declare-function pi-deck-menu "pi-deck-menu" ())
(declare-function pi-deck-answer-dialog "pi-deck-goto" (&optional agent))
(declare-function pi-deck-jump "pi-deck-goto" (agent))
(declare-function pi-deck-popup "pi-deck-menus" (menu &optional event))
(declare-function pi-deck-agent-menu "pi-deck-menus" (agent))
(declare-function pi-deck-space-menu "pi-deck-menus" (space))
(declare-function pi-deck-finished-worker-menu "pi-deck-menus" (record))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-define-key* "evil-core" (state keymap key def &rest bindings))

(defcustom pi-deck-sidebar-width 36
  "Width of the agent sidebar window."
  :type 'natnum
  :group 'pi-deck)

(defcustom pi-deck-sidebar t
  "Whether the agent sidebar is shown in every tab except the root tab.
`pi-deck-sidebar-toggle' changes it and saves the choice as a preference."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-sidebar-side 'left
  "Frame side of the agent sidebar."
  :type '(choice (const left) (const right))
  :group 'pi-deck)

(defconst pi-deck-overview-buffer-name "*pi-deck*"
  "Name of the root overview buffer.")

(defconst pi-deck-sidebar-buffer-name "*pi-deck-sidebar*"
  "Name of the sidebar buffer.")

(defvar-local pi-deck-overview--filter nil
  "Status shown by this view, or nil for all.")

(defvar-local pi-deck-overview--sort 'spaces
  "Agent order: `spaces' (space, tab, tree) or `priority'.")

(defvar-local pi-deck-overview--compact nil
  "Non-nil in the sidebar's compact view.")

(defvar-local pi-deck-overview--collapsed nil
  "Space names whose agents are hidden.")

(defvar-local pi-deck-overview--sheet-width nil
  "Width the key sheet was last laid out for, or nil without a sheet.")

(defvar-local pi-deck-overview--expanded-workers nil
  "Parent agent buffers whose closed workers are listed.")

(defvar-local pi-deck-overview--current-space nil
  "Space of the selected tab while rendering, highlighted in the sidebar.")

(defvar-local pi-deck-overview--shown nil
  "Agents shown in the selected tab while rendering, in bold in the sidebar.")

(defvar-local pi-deck-overview--shown-elsewhere nil
  "Agents shown only in other tabs while rendering, marked ◦ in the sidebar.")

(defface pi-deck-current-space '((t :inherit bold :underline t))
  "Face for the current space's name in the sidebar."
  :group 'pi-deck)

(defface pi-deck-button '((t :inherit shadow))
  "Face for the sidebar's ✕ and + buttons."
  :group 'pi-deck)

(defvar-keymap pi-deck-overview-row-map
  :doc "Mouse bindings of overview and sidebar rows."
  "<mouse-1>" #'pi-deck-overview-mouse-1
  "<mouse-2>" #'pi-deck-overview-mouse-show
  ;; Menus open on release: a text terminal menu opened on press picks
  ;; its first item at the release.
  "<down-mouse-3>" #'ignore
  "<mouse-3>" #'pi-deck-overview-mouse-3)

(defun pi-deck-overview--clickable (string help)
  "Return STRING as a clickable row with HELP shown on hover."
  (add-text-properties 0 (max 0 (1- (length string)))
                       (list 'mouse-face 'highlight 'help-echo help 'pointer 'hand
                             'keymap pi-deck-overview-row-map)
                       string)
  string)

(defun pi-deck-overview--button (label action help)
  "Return LABEL as a button running ACTION (see `pi-deck-overview-mouse-1').
HELP is shown on hover."
  (propertize label 'pi-deck-button action 'face 'pi-deck-button
              'mouse-face 'highlight 'help-echo help 'pointer 'hand))

(defun pi-deck-overview--right (button)
  "Return BUTTON aligned to the right edge of the window."
  ;; One column more than BUTTON: text terminals reserve the last one.
  (concat (propertize " " 'display `(space :align-to (- right ,(+ 2 (string-width button)))))
          button))

;;;; Rendering

(defun pi-deck-overview--tree (agents)
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

(defun pi-deck-overview--root (agent agents)
  "Return AGENT's outermost ancestor among AGENTS (AGENT itself if none)."
  (let ((seen (list agent)))
    (while-let ((parent (cl-find (pi-deck-agent-parent agent) agents
                                 :key #'pi-deck-agent-buffer))
                ((not (memq parent seen))))
      (push parent seen)
      (setq agent parent))
    agent))

(defun pi-deck-overview--agent-line (agent depth location)
  "Return the line for AGENT at tree DEPTH; LOCATION is shown when non-nil."
  (let* ((status (pi-deck-agent-status agent))
         (name (pi-deck-agent-name agent))
         (detail (pi-deck-agent-detail agent))
         (indent (make-string (* 2 depth) ?\s))
         (line
          (if pi-deck-overview--compact
              (concat "   " indent (pi-deck-status-icon status) " "
                      (truncate-string-to-width
                       (concat (if (memq agent pi-deck-overview--shown) (propertize name 'face 'bold) name)
                               (when (memq agent pi-deck-overview--shown-elsewhere)
                                 (propertize " ◦" 'face 'shadow
                                             'help-echo "shown in another tab"))
                               (when detail (propertize (concat "  " detail) 'face 'shadow)))
                       (max 4 (- pi-deck-sidebar-width (length indent) 9)) nil nil "…")
                      (pi-deck-overview--right
                       (pi-deck-overview--button "✕" 'stop "mouse-1: stop (asks when busy)")))
            (concat "    " indent (pi-deck-status-icon status) " "
                    (propertize (format "%-8s" status) 'face (pi-deck-status-face status))
                    (format " %-24s" (concat (if (> depth 0) "└ " "")
                                             (truncate-string-to-width name 22)))
                    (propertize (format " %-6s" (if (eq (pi-deck-agent-kind agent) 'pi) "pi" "shell"))
                                'face 'shadow)
                    (format " %-22s" (truncate-string-to-width
                                      (or (pi-deck-agent-model agent) "") 22))
                    (propertize (format " %4s" (pi-deck-age (pi-deck-agent-changed agent)))
                                'face 'shadow)
                    (if location (concat "  " location) "")
                    (if detail (propertize (concat "  " detail) 'face 'pi-deck-blocked) "")))))
    (pi-deck-overview--clickable
     (propertize (concat line "\n") 'pi-deck-agent agent)
     "mouse-1: jump to this agent; mouse-2: show it beside; mouse-3: menu")))

(defun pi-deck-overview--insert-finished (agent depth)
  "Insert AGENT's closed workers at tree DEPTH: a count, or the list."
  (when-let* ((buffer (pi-deck-agent-buffer agent))
              (records (and (null pi-deck-overview--filter)
                            (pi-deck-finished-workers buffer))))
    (let ((indent (concat (if pi-deck-overview--compact "   " "    ")
                          (make-string (* 2 depth) ?\s)))
          (expanded (memq buffer pi-deck-overview--expanded-workers)))
      (insert (pi-deck-overview--clickable
               (propertize
                (concat indent (if expanded "▾ " "▸ ")
                        (propertize (format "%d finished worker%s" (length records)
                                            (if (cdr records) "s" ""))
                                    'face 'shadow)
                        "\n")
                'pi-deck-finished buffer)
               "mouse-1: show or hide closed workers"))
      (when expanded
        (dolist (record records)
          (insert (pi-deck-overview--clickable
                   (propertize
                    (concat indent "  ✓ "
                            (format "%-24s" (truncate-string-to-width (plist-get record :name) 22))
                            (propertize (format " %4s  %s" (pi-deck-age (plist-get record :time))
                                                (abbreviate-file-name (plist-get record :session-file)))
                                        'face 'shadow)
                            "\n")
                    'pi-deck-worker-record record)
                   "mouse-1: read this worker's session; mouse-3: menu")))))))

(defun pi-deck-overview--visible-p (agent)
  "Return non-nil when AGENT passes this view's filter."
  (or (null pi-deck-overview--filter)
      (eq (pi-deck-agent-status agent) pi-deck-overview--filter)))

(defun pi-deck-overview--space-header (space status count)
  "Return the header line for SPACE with rolled-up STATUS and agent COUNT.
Its arrow folds, its name visits the space and its `+' opens the space's
menu."
  (let* ((dir (pi-deck-space-dir space))
         (name (or space "other"))
         (branch (and dir (not pi-deck-overview--compact) (pi-deck-git-branch dir)))
         (collapsed (member name pi-deck-overview--collapsed)))
    (pi-deck-overview--clickable
     (propertize
      (concat (if pi-deck-overview--compact " " "")
              (pi-deck-overview--button (if collapsed "▸ " "▾ ") 'fold "mouse-1: fold or unfold")
              (pi-deck-status-icon (or status 'idle)) " "
              (propertize name 'face (if (and space (equal space pi-deck-overview--current-space))
                                         'pi-deck-current-space
                                       'bold))
              (propertize (format " (%d)" count) 'face 'shadow)
              (when (and dir (not pi-deck-overview--compact))
                (propertize (concat "  " (abbreviate-file-name dir)) 'face 'shadow))
              (when branch (propertize (concat "  " branch) 'face 'font-lock-constant-face))
              (when space
                (pi-deck-overview--right
                 (pi-deck-overview--button "+" 'space-menu "mouse-1: new agent, shell or tab")))
              "\n")
      'pi-deck-space name)
     "mouse-1: switch to this space; mouse-3: menu")))

(defun pi-deck-overview--insert-tab-groups (agents space tab-index)
  "Insert SPACE's AGENTS grouped by the tab showing them, using TAB-INDEX.
Groups follow tab order; a tab of another space is labelled with it, and
agents no tab shows come last.  Workers follow their outermost listed
ancestor."
  (let ((groups nil))
    (dolist (agent agents)
      (let* ((root (pi-deck-overview--root agent agents))
             (tab (pi-deck--agent-tab root tab-index))
             (key (and tab (nth 1 tab)))
             (cell (assoc key groups)))
        (if cell (setcdr cell (append (cdr cell) (list agent)))
          (push (list key agent) groups))))
    (setq groups (sort groups :key (lambda (group) (or (car group) most-positive-fixnum))))
    (dolist (group groups)
      (let* ((entry (and (car group) (nth (1- (car group)) tab-index)))
             (group-space (and entry (pi-deck--tab-group (car entry)))))
        (insert (propertize
                 (cond ((null entry) "  not shown in a tab\n")
                       ((equal group-space space)
                        (format "  tab %s\n" (pi-deck-tab-label (car entry) (car group))))
                       (t (format "  tab %s (in %s)\n" (pi-deck-tab-label (car entry) (car group))
                                  (or group-space "other"))))
                 'face 'shadow)))
      (pi-deck-overview--insert-tree (cdr group)))))

(defun pi-deck-overview--insert-tree (agents)
  "Insert AGENTS nested under their parents, each with its closed workers."
  (dolist (item (pi-deck-overview--tree agents))
    (insert (pi-deck-overview--agent-line (car item) (cdr item) nil))
    (pi-deck-overview--insert-finished (car item) (1+ (cdr item)))))

(defun pi-deck-overview--insert-spaces (agents tab-index)
  "Insert AGENTS by home space using TAB-INDEX.
The sidebar lists each space's agents in their stable creation order; the
overview groups them by the tab showing them."
  (let* ((names (append (pi-deck-space-names) (list nil)))
         (by-space (mapcar (lambda (name)
                             (cons name (cl-remove-if-not
                                         (lambda (agent)
                                           (equal (pi-deck-agent-space agent tab-index) name))
                                         agents)))
                           names)))
    (dolist (entry by-space)
      (let* ((space (car entry))
             (members (cdr entry))
             (shown (cl-remove-if-not #'pi-deck-overview--visible-p members)))
        (when (and (not (equal space pi-deck-root-name))
                   (or shown (and (null pi-deck-overview--filter) space)))
          (insert (pi-deck-overview--space-header
                   space (pi-deck-rollup (mapcar #'pi-deck-agent-status members))
                   (length members)))
          (unless (member (or space "other") pi-deck-overview--collapsed)
            (if pi-deck-overview--compact
                (pi-deck-overview--insert-tree shown)
              (pi-deck-overview--insert-tab-groups shown space tab-index))))))))

(defun pi-deck-overview--insert-priority (agents tab-index)
  "Insert AGENTS by attention priority, then most recent change."
  (dolist (agent (sort (cl-remove-if-not #'pi-deck-overview--visible-p agents)
                       (lambda (a b)
                         (let ((pa (pi-deck-status-priority (pi-deck-agent-status a)))
                               (pb (pi-deck-status-priority (pi-deck-agent-status b))))
                           (if (= pa pb)
                               (> (pi-deck-agent-changed a) (pi-deck-agent-changed b))
                             (> pa pb))))))
    (let ((tab (pi-deck--agent-tab agent tab-index)))
      (insert (pi-deck-overview--agent-line
               agent 0
               (propertize (concat (or (pi-deck-agent-space agent tab-index) "other")
                                   (if tab (format "/%s" (pi-deck-tab-label (car tab) (nth 1 tab))) ""))
                           'face 'shadow))))))

(defun pi-deck-summary-string (&optional summary)
  "Return a status summary like \"◉ 2 blocked · ● 1 done\" for SUMMARY."
  (mapconcat (lambda (cell)
               (concat (pi-deck-status-icon (car cell))
                       (propertize (format " %d %s" (cdr cell) (car cell))
                                   'face (pi-deck-status-face (car cell)))))
             (or summary (pi-deck-summary))
             (propertize " · " 'face 'shadow)))

(defun pi-deck-overview--render ()
  "Rebuild the current overview or sidebar buffer, keeping point on its agent."
  (let* ((inhibit-read-only t)
         (agent (get-text-property (point) 'pi-deck-agent))
         (space (get-text-property (point) 'pi-deck-space))
         (line (line-number-at-pos))
         (window-start (and (get-buffer-window) (window-start (get-buffer-window))))
         (tab-index (pi-deck-tab-index))
         (agents (pi-deck-agents)))
    (setq pi-deck-overview--current-space (pi-deck-current-space)
          pi-deck-overview--shown
          (pi-deck--agents-shown-by (mapcar #'window-buffer (window-list nil 'no-minibuf)))
          pi-deck-overview--shown-elsewhere
          (cl-remove-if (lambda (agent) (or (memq agent pi-deck-overview--shown)
                                            (not (pi-deck--agent-tab agent tab-index))))
                        agents))
    (erase-buffer)
    (unless pi-deck-overview--compact
      (insert (propertize "pi-deck" 'face 'bold) "  "
              (if agents (pi-deck-summary-string) (propertize "no agents" 'face 'shadow))
              "\n")
      (dolist (info (pi-deck-daemon-info-lines))
        (insert (propertize info 'face 'shadow) "\n"))
      (insert (propertize
               (format "filter: %s · sort: %s · RET/click jump or visit space  o/mouse-2 beside  mouse-3 menu  TAB fold  b/w/i/d/D/a filter  s sort  A answer  K stop  + space  N agent  E shell  h keys  m menu\n\n"
                       (or pi-deck-overview--filter "all") pi-deck-overview--sort)
               'face 'shadow)))
    (when pi-deck-overview--compact
      (insert (pi-deck-overview--clickable
               (propertize " ⌂ Overview\n" 'pi-deck-action #'pi-deck-root-show)
               "mouse-1: overview of all agents")
              "\n"))
    (when (and pi-deck-overview--compact pi-deck-overview--filter)
      (insert (propertize (format " filter: %s\n" pi-deck-overview--filter) 'face 'shadow)))
    (if (eq pi-deck-overview--sort 'priority)
        (pi-deck-overview--insert-priority agents tab-index)
      (pi-deck-overview--insert-spaces agents tab-index))
    (when pi-deck-overview--compact
      (insert "\n" (pi-deck-overview--clickable
                    (propertize (concat " " (propertize "+ New space" 'face 'link) "\n")
                                'pi-deck-action #'pi-deck-create-space)
                    "mouse-1: create a space for a project directory")))
    (setq pi-deck-overview--sheet-width nil)
    (when (and pi-deck-key-sheet (not pi-deck-overview--compact))
      (let ((width (if-let* ((window (get-buffer-window nil t)))
                       (window-width window)
                     (frame-width))))
        (when-let* ((sheet (pi-deck-key-sheet-string (1- width))))
          (setq pi-deck-overview--sheet-width width)
          (insert "\n" sheet))))
    (goto-char (point-min))
    (let ((match (or (and agent (text-property-any (point-min) (point-max) 'pi-deck-agent agent))
                     (and space (text-property-any (point-min) (point-max) 'pi-deck-space space)))))
      (if match (goto-char match) (forward-line (1- line))))
    (when window-start
      (set-window-start (get-buffer-window) (min window-start (point-max)) t))))

(defun pi-deck-overview-refresh (&optional buffer)
  "Rebuild BUFFER (default current) if it is an overview or sidebar.
Interactively, also look up the key sheet's keys again."
  (interactive)
  (when (called-interactively-p 'any) (pi-deck-key-sheet-invalidate))
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'pi-deck-overview-mode)
      (pi-deck-overview--render))))

(defun pi-deck-overview-refresh-visible ()
  "Rebuild the overview and sidebar where they are visible."
  (dolist (name (list pi-deck-overview-buffer-name pi-deck-sidebar-buffer-name))
    (when-let* ((buffer (get-buffer name)))
      (when (get-buffer-window buffer 'visible)
        (pi-deck-overview-refresh buffer)))))

;;;; Commands

(defun pi-deck-overview--agent-at-point ()
  "Return the agent on this line or signal a user error."
  (or (get-text-property (line-beginning-position) 'pi-deck-agent)
      (user-error "No agent on this line")))

(defun pi-deck-overview-visit-space (space)
  "Unfold SPACE in this view and switch to its most recent tab."
  (setq pi-deck-overview--collapsed (delete space pi-deck-overview--collapsed))
  (if (member space (pi-deck-space-names))
      (pi-deck-select-space space)
    (pi-deck-overview--render)))

(defun pi-deck-overview-jump ()
  "Act on this line: run its key sheet entry or action, visit its space,
open its closed worker, show its closed workers, or jump to its agent."
  (interactive)
  (let ((position (line-beginning-position)))
    (cond
     ((get-text-property (point) 'pi-deck-key-command) (pi-deck-key-sheet-run))
     ((get-text-property position 'pi-deck-action)
      (call-interactively (get-text-property position 'pi-deck-action)))
     ((get-text-property position 'pi-deck-worker-record)
      (pi-deck-open-finished-worker (get-text-property position 'pi-deck-worker-record)))
     ((get-text-property position 'pi-deck-space)
      (pi-deck-overview-visit-space (get-text-property position 'pi-deck-space)))
     ((get-text-property position 'pi-deck-finished) (pi-deck-overview-toggle))
     (t (pi-deck-jump (pi-deck-overview--agent-at-point))))))

(defun pi-deck-overview-mouse-1 (event)
  "Act on what EVENT clicked: a ✕, + or fold button, else the line.
Buttons leave the selected window alone; lines act like \\[pi-deck-overview-jump]."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (posn-window posn))
         (position (posn-point posn)))
    (with-current-buffer (window-buffer window)
      (let ((row (save-excursion (goto-char position) (line-beginning-position))))
        (pcase (get-text-property position 'pi-deck-button)
          ('fold (with-selected-window window
                   (goto-char position)
                   (pi-deck-overview-toggle)))
          ('stop (pi-deck-stop (get-text-property row 'pi-deck-agent) event))
          ('space-menu (pi-deck-popup (pi-deck-space-menu (get-text-property row 'pi-deck-space))
                                      event))
          (_ (mouse-set-point event)
             (pi-deck-overview-jump)))))))

(defun pi-deck-overview-mouse-show (event)
  "Show the agent clicked in EVENT beside the current pane."
  (interactive "e")
  (let ((posn (event-start event)))
    (when-let* ((agent (with-current-buffer (window-buffer (posn-window posn))
                         (get-text-property (posn-point posn) 'pi-deck-agent))))
      (pi-deck-show-beside agent))))

(defun pi-deck-overview-mouse-3 (event)
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

(defun pi-deck-overview-toggle ()
  "Fold the space at point, or show or hide the closed workers at point."
  (interactive)
  (if-let* ((parent (get-text-property (line-beginning-position) 'pi-deck-finished)))
      (progn
        (setq pi-deck-overview--expanded-workers
              (if (memq parent pi-deck-overview--expanded-workers)
                  (delq parent pi-deck-overview--expanded-workers)
                (cons parent pi-deck-overview--expanded-workers)))
        (pi-deck-overview--render))
    (pi-deck-overview-toggle-space)))

(defun pi-deck-overview-show ()
  "Show the agent at point beside the current pane, keeping this view selected."
  (interactive)
  (save-selected-window
    (pi-deck-show-beside (pi-deck-overview--agent-at-point))))

(defun pi-deck-overview-toggle-space (&optional space)
  "Collapse or expand SPACE (default: the space at point)."
  (interactive)
  (let ((space (or space (get-text-property (line-beginning-position) 'pi-deck-space)
                   (user-error "No space on this line"))))
    (setq pi-deck-overview--collapsed
          (if (member space pi-deck-overview--collapsed)
              (delete space pi-deck-overview--collapsed)
            (cons space pi-deck-overview--collapsed)))
    (pi-deck-overview--render)))

(defun pi-deck-overview-filter (status)
  "Show only agents with STATUS; nil shows all."
  (interactive
   (list (pcase (event-basic-type last-command-event)
           (?b 'blocked) (?w 'working) (?i 'idle) (?d 'done) (?D 'done) (?u 'unknown) (_ nil))))
  (setq pi-deck-overview--filter status)
  (pi-deck-overview--render)
  (message "Filter: %s" (or status "all")))

(defun pi-deck-overview-toggle-sort ()
  "Toggle between space order and attention priority."
  (interactive)
  (setq pi-deck-overview--sort (if (eq pi-deck-overview--sort 'spaces) 'priority 'spaces))
  (pi-deck-overview--render))

(defun pi-deck-overview-next (n)
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

(defun pi-deck-overview-previous (n)
  "Move to the Nth previous agent line."
  (interactive "p")
  (pi-deck-overview-next (- n)))

(defun pi-deck-overview-stop ()
  "Stop the agent at point; asks first when it is working or blocked."
  (interactive)
  (pi-deck-stop (pi-deck-overview--agent-at-point))
  (pi-deck-overview--render))

(defun pi-deck-overview-answer ()
  "Answer the dialog of the agent at point."
  (interactive)
  (pi-deck-answer-dialog (pi-deck-overview--agent-at-point)))

(defun pi-deck-overview--space-at-point ()
  "Return the space of this line (header or agent), or nil."
  (let ((space (or (get-text-property (line-beginning-position) 'pi-deck-space)
                   (when-let* ((agent (get-text-property (line-beginning-position)
                                                         'pi-deck-agent)))
                     (pi-deck-agent-space agent)))))
    (and space (member space (cdr (pi-deck-space-names))) space)))

(defun pi-deck-overview-new-agent ()
  "Start a Pi agent in the space at point (else a space read), and show it."
  (interactive)
  (when-let* ((space (pi-deck-overview--space-at-point)))
    (pi-deck-select-space space))
  (call-interactively #'pi-deck-new-agent))

(defun pi-deck-overview-new-shell ()
  "Open a shell in the space at point (else a space read), and show it."
  (interactive)
  (when-let* ((space (pi-deck-overview--space-at-point)))
    (pi-deck-select-space space))
  (call-interactively #'pi-deck-new-shell))

(defun pi-deck-key-sheet-toggle ()
  "Show or hide the overview's key sheet, remembering the choice."
  (interactive)
  (setq pi-deck-key-sheet (not pi-deck-key-sheet))
  (when-let* ((buffer (get-buffer pi-deck-overview-buffer-name)))
    (pi-deck-overview-refresh buffer))
  (with-demoted-errors "pi-deck: %S" (pi-deck-save-preference 'pi-deck-key-sheet)))

(defun pi-deck-overview-quit ()
  "Leave the overview: hide the sidebar, or return from the root tab."
  (interactive)
  (if pi-deck-overview--compact
      (pi-deck-sidebar-toggle)
    (pi-deck-root)))

(defvar-keymap pi-deck-overview-mode-map
  :doc "Keymap for the pi-deck overview and sidebar."
  :parent special-mode-map
  "RET" #'pi-deck-overview-jump
  "o" #'pi-deck-overview-show
  "TAB" #'pi-deck-overview-toggle
  "g" #'pi-deck-overview-refresh
  "b" #'pi-deck-overview-filter
  "w" #'pi-deck-overview-filter
  "i" #'pi-deck-overview-filter
  "d" #'pi-deck-overview-filter
  "D" #'pi-deck-overview-filter
  "u" #'pi-deck-overview-filter
  "a" #'pi-deck-overview-filter
  "s" #'pi-deck-overview-toggle-sort
  "n" #'pi-deck-overview-next
  "p" #'pi-deck-overview-previous
  "j" #'pi-deck-overview-next
  "k" #'pi-deck-overview-previous
  "A" #'pi-deck-overview-answer
  "K" #'pi-deck-overview-stop
  "+" #'pi-deck-create-space
  "N" #'pi-deck-overview-new-agent
  "E" #'pi-deck-overview-new-shell
  "h" #'pi-deck-key-sheet-toggle
  "m" #'pi-deck-menu
  "?" #'pi-deck-menu
  "q" #'pi-deck-overview-quit)

(defun pi-deck-overview--on-resize (window)
  "Lay the key sheet out again when WINDOW's width changed."
  (with-current-buffer (window-buffer window)
    (when (and pi-deck-overview--sheet-width
               (/= (window-width window) pi-deck-overview--sheet-width))
      (pi-deck-overview--render))))

(define-derived-mode pi-deck-overview-mode special-mode "Pi-Deck"
  "Overview of Pi agents, spaces and tabs.
\\{pi-deck-overview-mode-map}"
  (setq-local truncate-lines t)
  (add-hook 'window-size-change-functions #'pi-deck-overview--on-resize nil t)
  (hl-line-mode 1))

;; Evil users stay in normal state, so leader keys, `gg' and `/' keep
;; working.  The view's own keys go into its normal-state keymap, which
;; takes precedence over Evil's and evil-collection's `special-mode' ones;
;; refresh moves from `g' to `gr' to leave the `g' prefix intact.
(defun pi-deck-overview--evil-setup ()
  "Set up Evil normal-state bindings for the overview."
  (evil-set-initial-state 'pi-deck-overview-mode 'normal)
  (let (bindings)
    (map-keymap-internal
     (lambda (event command)
       (unless (eq event ?g)
         (setq bindings (append bindings (list (vector event) command)))))
     pi-deck-overview-mode-map)
    (apply #'evil-define-key* 'normal pi-deck-overview-mode-map
           "gr" #'pi-deck-overview-refresh bindings)))

(with-eval-after-load 'evil (pi-deck-overview--evil-setup))

(defun pi-deck-overview-buffer (&optional compact)
  "Return the overview buffer, or the sidebar buffer when COMPACT."
  (let ((name (if compact pi-deck-sidebar-buffer-name pi-deck-overview-buffer-name)))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (pi-deck-overview-mode)
          (setq pi-deck-overview--compact compact)
          (when compact (setq cursor-type nil))
          (pi-deck-overview--render)
          (current-buffer)))))

;;;; Root tab

(defun pi-deck-root-ensure (&optional frame)
  "Ensure FRAME has the pinned root tab first, without selecting it."
  (with-selected-frame (or frame (selected-frame))
    (tab-bar-mode 1)
    (unless (cl-find-if #'pi-deck-root-tab-p (pi-deck--tabs))
      (let ((current (1+ (tab-bar--current-tab-index)))
            (tab-bar-new-tab-group nil)
            (tab-bar-new-tab-choice (pi-deck-overview-buffer)))
        (tab-bar-new-tab-to 1)
        (pi-deck--set-tab-group pi-deck-root-name)
        (tab-bar-rename-tab "overview")
        (pi-deck--single-window)
        (switch-to-buffer (pi-deck-overview-buffer))
        (pi-deck--sidebar-sync)
        (tab-bar-select-tab (1+ current))))))

(defun pi-deck-root-show ()
  "Select the root overview tab."
  (interactive)
  (pi-deck-root-ensure)
  (pi-deck--select-tab-index
   (1+ (cl-position-if #'pi-deck-root-tab-p (pi-deck--tabs))))
  (unless (get-buffer-window (pi-deck-overview-buffer))
    (pi-deck--single-window)
    (switch-to-buffer (pi-deck-overview-buffer)))
  (select-window (get-buffer-window (pi-deck-overview-buffer)))
  (pi-deck-overview-refresh (pi-deck-overview-buffer)))

(defun pi-deck-land-on-root ()
  "Show the root tab in place of the selected frame's unused initial tab.
The unused tab (see `pi-deck--unused-tab-p') is closed."
  (when (and (memq (selected-frame) (pi-deck--client-frames))
             (pi-deck--unused-tab-p))
    (let ((unused (progn (pi-deck-root-ensure) (1+ (tab-bar--current-tab-index)))))
      (pi-deck-root-show)
      (pi-deck--close-tabs-internally (list unused)))))

;;;###autoload
(defun pi-deck-initial-buffer ()
  "Return the overview buffer, for `initial-buffer-choice'.
Without `pi-deck-mode', return *scratch*."
  (if (bound-and-true-p pi-deck-mode)
      (pi-deck-overview-buffer)
    (get-scratch-buffer-create)))

;;;###autoload
(defun pi-deck-root ()
  "Toggle between the root overview tab and the previously used tab."
  (interactive)
  (if (and (pi-deck-root-tab-p (tab-bar--current-tab-find))
           (> (length (pi-deck--tabs)) 1))
      (tab-bar-switch-to-recent-tab)
    (pi-deck-root-show)))

;;;; Sidebar

(defun pi-deck--sidebar-show ()
  "Show the sidebar in the selected frame's current tab."
  (let ((window (display-buffer-in-side-window
                 (pi-deck-overview-buffer t)
                 `((side . ,pi-deck-sidebar-side) (slot . 0)
                   (window-width . ,pi-deck-sidebar-width)
                   (preserve-size . (t . nil))
                   (window-parameters . ((no-delete-other-windows . t)))))))
    (set-window-dedicated-p window t)
    (pi-deck-overview-refresh (window-buffer window))
    window))

(defun pi-deck--sidebar-sync (&rest _)
  "Show or hide the sidebar in the current tab according to its toggle."
  (let ((window (get-buffer-window pi-deck-sidebar-buffer-name)))
    (cond ((and pi-deck-sidebar (not window)
                (not (pi-deck-root-tab-p (tab-bar--current-tab-find))))
           (pi-deck--sidebar-show))
          ((and window (or (not pi-deck-sidebar)
                           (pi-deck-root-tab-p (tab-bar--current-tab-find))))
           (delete-window window)))))

;;;###autoload
(defun pi-deck-sidebar-toggle ()
  "Toggle the agent sidebar in all tabs, remembering the choice."
  (interactive)
  (setq pi-deck-sidebar (not pi-deck-sidebar))
  (pi-deck--sidebar-sync)
  (with-demoted-errors "pi-deck: %S" (pi-deck-save-preference 'pi-deck-sidebar)))

(provide 'pi-deck-overview)
;;; pi-deck-overview.el ends here
