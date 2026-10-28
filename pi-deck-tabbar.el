;;; pi-deck-tabbar.el --- Tab bar status cell, icons and preferences -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Adds a right-aligned status summary ("◉ 2 blocked · ● 1 done") and
;; rolled-up status icons before tab and space (group) names.  Tab bar
;; preferences toggled from the root menu persist in their own file, so the
;; user's init files are never rewritten.

;;; Code:

(require 'tab-bar)
(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-overview)

(declare-function pi-deck-next-attention "pi-deck-goto" ())

(defcustom pi-deck-summary-statuses '(blocked done working)
  "Statuses counted in the tab bar status cell."
  :type '(set (const blocked) (const done) (const working) (const idle) (const unknown))
  :group 'pi-deck)

(defcustom pi-deck-tab-bar-groups t
  "Whether the tab bar shows tabs grouped by space."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-tab-bar-status-cell t
  "Whether the tab bar shows the agent status summary."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-tab-bar-icons t
  "Whether tab and space names get rolled-up status icons."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-preferences-file
  (locate-user-emacs-file "var/pi-deck/preferences.eld")
  "File storing tab bar preferences saved with `pi-deck-save-preferences'."
  :type 'file
  :group 'pi-deck)

(defconst pi-deck-preference-variables
  '(pi-deck-tab-bar-groups pi-deck-tab-bar-status-cell pi-deck-tab-bar-icons
    tab-bar-show-inactive-group-tabs tab-bar-tab-hints tab-bar-show
    pi-deck-sidebar pi-deck-sidebar-side pi-deck-sidebar-width pi-deck-reveal-action)
  "Variables saved by `pi-deck-save-preferences'.")

(defvar pi-deck--saved-tab-bar-settings nil
  "Tab bar settings from before `pi-deck-mode', restored when it is disabled.")

(defun pi-deck-tab-bar-status-string ()
  "Return the status summary shown in the tab bar, or nil."
  (when pi-deck-tab-bar-status-cell
    (let ((summary (cl-remove-if-not (lambda (cell) (memq (car cell) pi-deck-summary-statuses))
                                     (pi-deck-summary))))
      (when summary
        (concat (pi-deck-summary-string summary) " ")))))

(defun pi-deck-tab-bar-format-status ()
  "Return the tab bar status cell; clicking it jumps to the next attention."
  (when-let* ((status (pi-deck-tab-bar-status-string)))
    `((pi-deck-status menu-item ,status pi-deck-tab-bar-status-click
                      :help "mouse-1: next agent needing attention, else the overview"))))

(defun pi-deck-tab-bar-status-click ()
  "Jump to the next agent needing attention, or show the overview."
  (interactive)
  (if (cl-some (lambda (agent) (memq (pi-deck-agent-status agent) '(blocked done)))
               (pi-deck-agents))
      (pi-deck-next-attention)
    (pi-deck-root-show)))

(defun pi-deck-tab-name-icon (name tab _i)
  "Prefix tab NAME with TAB's rolled-up status icon."
  (let ((status (and pi-deck-tab-bar-icons (pi-deck-tab-status tab))))
    (if (memq status '(blocked done working))
        (concat (pi-deck-status-icon status) " " name)
      name)))

(defun pi-deck-tab-group-format (tab i &optional current-p)
  "Format TAB's group name at I, CURRENT-P as in the default, with an icon."
  (let* ((formatted (tab-bar-tab-group-format-default tab i current-p))
         (status (and pi-deck-tab-bar-icons
                      (cdr (assoc (pi-deck--tab-group tab) (pi-deck-space-statuses))))))
    (if (memq status '(blocked done working))
        (concat (pi-deck-status-icon status) " " formatted)
      formatted)))

(defun pi-deck--tab-bar-format ()
  "Return the `tab-bar-format' for the current preferences."
  (append (list (if pi-deck-tab-bar-groups #'tab-bar-format-tabs-groups #'tab-bar-format-tabs)
                #'tab-bar-separator
                #'tab-bar-format-align-right
                #'pi-deck-tab-bar-format-status)))

(defun pi-deck-tab-name ()
  "Name a tab after the agent in its selected window, else after its buffer.
Pi agents are \"pi:NAME\", terminals their buffer name without stars.
Explicitly renamed tabs keep their names."
  (let* ((window (or (minibuffer-selected-window)
                     (and (window-minibuffer-p) (get-mru-window))
                     (selected-window)))
         ;; Side windows such as the sidebar do not name tabs.
         (window (if (window-parameter window 'window-side)
                     (or (get-mru-window nil nil t) window)
                   window))
         (buffer (window-buffer window))
         (agent (pi-deck-agent (or (pilish-session-chat-buffer buffer) buffer))))
    (cond ((null agent) (tab-bar-tab-name-current))
          ((eq (pi-deck-agent-kind agent) 'pi) (concat "pi:" (pi-deck-agent-name agent)))
          (t (pi-deck-agent-name agent)))))

(defun pi-deck-tab-bar-apply ()
  "Apply pi-deck's tab bar settings."
  (setq tab-bar-format (pi-deck--tab-bar-format)
        tab-bar-tab-group-format-function #'pi-deck-tab-group-format
        tab-bar-tab-name-function #'pi-deck-tab-name)
  (add-to-list 'tab-bar-tab-name-format-functions #'pi-deck-tab-name-icon)
  (force-mode-line-update t))

(defun pi-deck-tab-bar-enable ()
  "Install pi-deck's tab bar, remembering the previous settings."
  (unless pi-deck--saved-tab-bar-settings
    (setq pi-deck--saved-tab-bar-settings
          (list (cons 'tab-bar-format tab-bar-format)
                (cons 'tab-bar-tab-group-format-function tab-bar-tab-group-format-function)
                (cons 'tab-bar-tab-name-function tab-bar-tab-name-function)
                (cons 'tab-bar-tab-name-format-functions tab-bar-tab-name-format-functions))))
  (pi-deck-tab-bar-apply)
  (tab-bar-mode 1))

(defun pi-deck-tab-bar-disable ()
  "Restore the tab bar settings from before pi-deck."
  (dolist (cell pi-deck--saved-tab-bar-settings)
    (set (car cell) (cdr cell)))
  (setq pi-deck--saved-tab-bar-settings nil)
  (force-mode-line-update t))

(defmacro pi-deck--define-toggle (name variable doc)
  "Define command NAME toggling VARIABLE (DOC) and reapplying the tab bar."
  `(defun ,name ()
     ,doc
     (interactive)
     (setq ,variable (not ,variable))
     (pi-deck-tab-bar-apply)
     (message "%s: %s" ',variable (if ,variable "on" "off"))))

(pi-deck--define-toggle pi-deck-toggle-tab-groups pi-deck-tab-bar-groups
  "Toggle grouping tabs by space in the tab bar.")
(pi-deck--define-toggle pi-deck-toggle-status-cell pi-deck-tab-bar-status-cell
  "Toggle the tab bar status summary.")
(pi-deck--define-toggle pi-deck-toggle-tab-icons pi-deck-tab-bar-icons
  "Toggle status icons before tab and space names.")
(pi-deck--define-toggle pi-deck-toggle-inactive-group-tabs tab-bar-show-inactive-group-tabs
  "Toggle showing the tabs of other spaces.")
(pi-deck--define-toggle pi-deck-toggle-tab-hints tab-bar-tab-hints
  "Toggle tab numbers in the tab bar.")

(defun pi-deck-toggle-tab-bar ()
  "Toggle showing the tab bar (tabs remain)."
  (interactive)
  (customize-set-variable 'tab-bar-show (if tab-bar-show nil t))
  (unless tab-bar-show
    (dolist (frame (frame-list)) (set-frame-parameter frame 'tab-bar-lines 0)))
  (message "Tab bar %s" (if tab-bar-show "shown" "hidden")))

(defun pi-deck--read-preferences ()
  "Return the saved preferences alist, or nil."
  (when (file-readable-p pi-deck-preferences-file)
    (with-temp-buffer
      (insert-file-contents pi-deck-preferences-file)
      (read (current-buffer)))))

(defun pi-deck--write-preferences (preferences)
  "Write the PREFERENCES alist to `pi-deck-preferences-file'."
  (make-directory (file-name-directory pi-deck-preferences-file) t)
  (with-file-modes #o600
    (with-temp-file pi-deck-preferences-file
      (let ((print-length nil) (print-level nil))
        (insert ";; pi-deck preferences -*- lisp-data -*-\n")
        (prin1 preferences (current-buffer))
        (insert "\n")))))

(defun pi-deck-save-preferences ()
  "Save tab bar and layout preferences to `pi-deck-preferences-file'."
  (interactive)
  (pi-deck--write-preferences
   (mapcar (lambda (variable) (cons variable (symbol-value variable)))
           pi-deck-preference-variables))
  (message "Saved pi-deck preferences to %s" pi-deck-preferences-file))

(defun pi-deck-save-preference (variable)
  "Save VARIABLE's current value, keeping the other saved preferences."
  (let ((preferences (pi-deck--read-preferences)))
    (setf (alist-get variable preferences) (symbol-value variable))
    (pi-deck--write-preferences preferences)))

(defun pi-deck-load-preferences ()
  "Load preferences saved by `pi-deck-save-preferences', if any."
  (dolist (cell (pi-deck--read-preferences))
    (when (memq (car cell) pi-deck-preference-variables)
      (set (car cell) (cdr cell)))))

(provide 'pi-deck-tabbar)
;;; pi-deck-tabbar.el ends here
