;;; pi-deck-overview.el --- Root overview tab and agent sidebar -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The root space's pinned tab shows `*pi-deck*': daemon status, then every
;; agent grouped by space and tab (workers nested under their parent), with
;; status filters and a priority sort.  The sidebar shows the same agents
;; compactly in a side window that follows tab switches.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-workers)

(declare-function pi-deck-save-preference "pi-deck-tabbar" (variable))
(declare-function pi-deck-daemon-info-lines "pi-deck-daemon" ())
(declare-function pi-deck-menu "pi-deck-menu" ())
(declare-function pi-deck-answer-dialog "pi-deck-goto" (&optional agent))
(declare-function pi-deck-jump "pi-deck-goto" (agent))
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

(defvar-local pi-deck-overview--expanded-workers nil
  "Parent agent buffers whose closed workers are listed.")

(defvar-keymap pi-deck-overview-row-map
  :doc "Mouse bindings of overview and sidebar rows."
  "<mouse-1>" #'pi-deck-overview-mouse-jump
  "<mouse-2>" #'pi-deck-overview-mouse-show)

(defun pi-deck-overview--clickable (string help)
  "Return STRING as a clickable row with HELP shown on hover."
  (add-text-properties 0 (max 0 (1- (length string)))
                       (list 'mouse-face 'highlight 'help-echo help
                             'keymap pi-deck-overview-row-map)
                       string)
  string)

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
              (concat " " indent (pi-deck-status-icon status) " " name
                      (when detail (propertize (concat "  " detail) 'face 'shadow)))
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
     "mouse-1: jump to this agent; mouse-2: show it in another window")))

(defun pi-deck-overview--insert-finished (agent depth)
  "Insert AGENT's closed workers at tree DEPTH: a count, or the list."
  (when-let* ((buffer (pi-deck-agent-buffer agent))
              (records (and (not pi-deck-overview--compact)
                            (null pi-deck-overview--filter)
                            (pi-deck-finished-workers buffer))))
    (let ((indent (concat "    " (make-string (* 2 depth) ?\s)))
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
                   "mouse-1: read this worker's session")))))))

(defun pi-deck-overview--visible-p (agent)
  "Return non-nil when AGENT passes this view's filter."
  (or (null pi-deck-overview--filter)
      (eq (pi-deck-agent-status agent) pi-deck-overview--filter)))

(defun pi-deck-overview--space-header (space status count)
  "Return the header line for SPACE with rolled-up STATUS and agent COUNT."
  (let* ((dir (pi-deck-space-dir space))
         (branch (and dir (not pi-deck-overview--compact) (pi-deck-git-branch dir)))
         (collapsed (member space pi-deck-overview--collapsed)))
    (pi-deck-overview--clickable
     (propertize
      (concat (if collapsed "▸ " "▾ ")
              (pi-deck-status-icon (or status 'idle)) " "
              (propertize (or space "other") 'face 'bold)
              (propertize (format " (%d)" count) 'face 'shadow)
              (when (and dir (not pi-deck-overview--compact))
                (propertize (concat "  " (abbreviate-file-name dir)) 'face 'shadow))
              (when branch (propertize (concat "  " branch) 'face 'font-lock-constant-face))
              "\n")
      'pi-deck-space (or space "other"))
     "mouse-1: fold or unfold this space")))

(defun pi-deck-overview--insert-spaces (agents tab-index)
  "Insert AGENTS grouped by space and tab using TAB-INDEX."
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
        (when (or shown (and (null pi-deck-overview--filter) space
                             (not (equal space pi-deck-root-name))))
          (insert (pi-deck-overview--space-header
                   space (pi-deck-rollup (mapcar #'pi-deck-agent-status members))
                   (length members)))
          (unless (member (or space "other") pi-deck-overview--collapsed)
            (let ((groups nil))
              ;; Group by displaying tab, keeping tab order; hidden agents
              ;; last.  Workers follow their outermost listed ancestor.
              (dolist (agent shown)
                (let* ((root (pi-deck-overview--root agent shown))
                       (tab (pi-deck--agent-tab root tab-index))
                       (key (and tab (nth 1 tab)))
                       (cell (assoc key groups)))
                  (if cell (setcdr cell (append (cdr cell) (list agent)))
                    (push (list key agent) groups))))
              (setq groups (sort groups :key (lambda (group) (or (car group) most-positive-fixnum))))
              (dolist (group groups)
                (unless pi-deck-overview--compact
                  (let ((entry (and (car group) (nth (1- (car group)) tab-index))))
                    (insert (propertize
                             (if entry
                                 (format "  tab %s\n" (pi-deck-tab-label (car entry) (car group)))
                               "  not shown in a tab\n")
                             'face 'shadow))))
                (dolist (item (pi-deck-overview--tree (cdr group)))
                  (insert (pi-deck-overview--agent-line (car item) (cdr item) nil))
                  (pi-deck-overview--insert-finished (car item) (1+ (cdr item))))))))))))

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
    (erase-buffer)
    (unless pi-deck-overview--compact
      (insert (propertize "pi-deck" 'face 'bold) "  "
              (if agents (pi-deck-summary-string) (propertize "no agents" 'face 'shadow))
              "\n")
      (dolist (info (pi-deck-daemon-info-lines))
        (insert (propertize info 'face 'shadow) "\n"))
      (insert (propertize
               (format "filter: %s · sort: %s · RET/click jump  o show  TAB fold  b/w/i/d/D/a filter  s sort  A answer  K stop  + space  N agent  E shell  m menu\n\n"
                       (or pi-deck-overview--filter "all") pi-deck-overview--sort)
               'face 'shadow)))
    (when (and pi-deck-overview--compact pi-deck-overview--filter)
      (insert (propertize (format " filter: %s\n" pi-deck-overview--filter) 'face 'shadow)))
    (if (eq pi-deck-overview--sort 'priority)
        (pi-deck-overview--insert-priority agents tab-index)
      (pi-deck-overview--insert-spaces agents tab-index))
    (goto-char (point-min))
    (let ((match (or (and agent (text-property-any (point-min) (point-max) 'pi-deck-agent agent))
                     (and space (text-property-any (point-min) (point-max) 'pi-deck-space space)))))
      (if match (goto-char match) (forward-line (1- line))))
    (when window-start
      (set-window-start (get-buffer-window) (min window-start (point-max)) t))))

(defun pi-deck-overview-refresh (&optional buffer)
  "Rebuild BUFFER (default current) if it is an overview or sidebar."
  (interactive)
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

(defun pi-deck-overview-jump ()
  "Act on this line: jump to its agent, open a closed worker, or fold."
  (interactive)
  (let ((position (line-beginning-position)))
    (cond
     ((get-text-property position 'pi-deck-worker-record)
      (pi-deck-open-finished-worker (get-text-property position 'pi-deck-worker-record)))
     ((or (get-text-property position 'pi-deck-space)
          (get-text-property position 'pi-deck-finished))
      (pi-deck-overview-toggle))
     (t (pi-deck-jump (pi-deck-overview--agent-at-point))))))

(defun pi-deck-overview-mouse-jump (event)
  "Act on the line clicked in EVENT, like \\[pi-deck-overview-jump]."
  (interactive "e")
  (mouse-set-point event)
  (pi-deck-overview-jump))

(defun pi-deck-overview-mouse-show (event)
  "Show the agent clicked in EVENT in another window."
  (interactive "e")
  (mouse-set-point event)
  (when (get-text-property (line-beginning-position) 'pi-deck-agent)
    (pi-deck-overview-show)))

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
  "Show the agent at point in another window, keeping this view selected."
  (interactive)
  (let ((agent (pi-deck-overview--agent-at-point)))
    (save-selected-window
      (if pi-deck-overview--compact
          (pi-deck--display-agent-in-window
           agent (or (get-mru-window nil nil t) (selected-window)))
        (display-buffer (pi-deck-agent-buffer agent) '(display-buffer-pop-up-window))))
    (pi-deck-mark-seen agent)))

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
  "Stop the agent at point after confirmation."
  (interactive)
  (let ((agent (pi-deck-overview--agent-at-point)))
    (when (yes-or-no-p (format "Stop %s %s? " (pi-deck-agent-kind agent)
                               (pi-deck-agent-name agent)))
      (pi-deck-stop-agent agent)
      (pi-deck-overview--render))))

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
  "m" #'pi-deck-menu
  "?" #'pi-deck-menu
  "q" #'pi-deck-overview-quit)

(define-derived-mode pi-deck-overview-mode special-mode "Pi-Deck"
  "Overview of Pi agents, spaces and tabs.
\\{pi-deck-overview-mode-map}"
  (setq-local truncate-lines t)
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

;;;###autoload
(defun pi-deck-root ()
  "Toggle between the root overview tab and the previously used tab."
  (interactive)
  (if (and (pi-deck-root-tab-p (tab-bar--current-tab-find))
           (> (length (pi-deck--tabs)) 1))
      (tab-bar-switch-to-recent-tab)
    (pi-deck-root-show)))

(defun pi-deck--prevent-root-close (tab _last-tab-p)
  "Prevent closing the root TAB (for `tab-bar-tab-prevent-close-functions')."
  (and (pi-deck-root-tab-p tab) (bound-and-true-p pi-deck-mode)
       (not (bound-and-true-p pi-deck--closing-root))))

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
