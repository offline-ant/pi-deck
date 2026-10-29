;;; pi-deck-tabbar.el --- Tab bar status cell, icons and preferences -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The tab bar shows one space at a time:
;;
;;   ⌂ │ pi ▾ │ ◐ main ✕ │ tests ✕ │ +            ◉ 1 blocked · ● 1 done
;;
;; ⌂ shows the overview (the hidden root tab), the space button opens a menu
;; of spaces, then come the current space's tabs with status icons and close
;; buttons, `+' (a menu for new tabs) and the right-aligned status summary.
;; Tabs are ordinary `tab-bar' items (`tab-N', `current-tab'), so tab-bar's
;; mouse handling keeps working: click selects, drag moves, mouse-2 and ✕
;; close (through `pi-deck-close-tab'), mouse-3 opens pi-deck's context
;; menu.  pi-deck's own buttons and menus act on release.
;;
;; Tab bar preferences toggled from the menu persist in their own file, so
;; the user's init files are never rewritten.

;;; Code:

(require 'tab-bar)
(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-close)
(require 'pi-deck-overview)

(declare-function pi-deck-next-attention "pi-deck-goto" ())
(declare-function pi-deck-space-menu "pi-deck-menus" (space))
(declare-function pi-deck--new-tab-menu-map "pi-deck-menus" ())
(declare-function pi-deck-tab-context-menu "pi-deck-menus" (number))
(declare-function pi-deck-popup "pi-deck-menus" (menu &optional event))
(declare-function pi-deck-spaces-menu "pi-deck-menus" (&optional event))

(defcustom pi-deck-summary-statuses '(blocked done working)
  "Statuses counted in the tab bar status cell."
  :type '(set (const blocked) (const done) (const working) (const idle) (const unknown))
  :group 'pi-deck)

(defcustom pi-deck-tab-bar-status-cell t
  "Whether the tab bar shows the agent status summary."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-tab-bar-icons t
  "Whether tabs and the space button get rolled-up status icons."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-tab-numbers nil
  "Whether tabs show their number within the space (as `SPC d 1…9' selects)."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-preferences-file
  (locate-user-emacs-file "var/pi-deck/preferences.eld")
  "File storing tab bar preferences saved with `pi-deck-save-preferences'."
  :type 'file
  :group 'pi-deck)

(defconst pi-deck-preference-variables
  '(pi-deck-tab-bar-status-cell pi-deck-tab-bar-icons pi-deck-tab-numbers tab-bar-show
    pi-deck-sidebar pi-deck-sidebar-side pi-deck-sidebar-width pi-deck-reveal-action
    pi-deck-key-sheet)
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

;;;; Items

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

(defun pi-deck--tab-bar-button (label face)
  "Return LABEL as a tab bar button string with FACE."
  (propertize label 'face face 'mouse-face 'tab-bar-tab-highlight 'pointer 'hand))

(defun pi-deck--icon-prefix (status)
  "Return STATUS's icon and a space when icons are on and STATUS matters."
  (if (and pi-deck-tab-bar-icons (memq status '(blocked done working)))
      (concat (pi-deck-status-icon status) " ")
    ""))

(defconst pi-deck--tab-close-button
  (propertize " ✕" 'close-tab t :help "Close tab; stops what only it shows")
  "Close button after tab names; `close-tab' makes tab-bar treat it as one.")

(defun pi-deck--format-tab-name (tab number)
  "Return TAB's tab bar label as NUMBER-th tab of its space."
  (let ((name (concat " " (pi-deck--icon-prefix (pi-deck-tab-status tab))
                      (if pi-deck-tab-numbers (format "%d " number) "")
                      (alist-get 'name tab)
                      pi-deck--tab-close-button " ")))
    (tab-bar-tab-name-format-mouse-face
     (tab-bar-tab-name-format-face name tab number) tab number)))

(defun pi-deck--tab-bar-space (tabs)
  "Return the space the tab bar shows for TABS.
That is the current tab's space; from the overview, the most recently used
space."
  (let ((current (tab-bar--current-tab-find tabs)))
    (if (not (pi-deck-root-tab-p current))
        (pi-deck--tab-group current)
      (let ((recent (car (sort (cl-remove-if #'pi-deck-root-tab-p (copy-sequence tabs))
                               :key (lambda (tab) (or (alist-get 'time tab) 0))
                               :reverse t))))
        (and recent (pi-deck--tab-group recent))))))

(defun pi-deck-tab-bar-format-space ()
  "Produce the overview button, the space button and the space's tabs."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (root (pi-deck-root-tab-p (tab-bar--current-tab-find tabs)))
         (space (pi-deck--tab-bar-space tabs))
         (status (and space (cdr (assoc space (pi-deck-space-statuses)))))
         (index 0) (number 0) items)
    (push `(pi-deck-home menu-item
                         ,(pi-deck--tab-bar-button " ⌂ " (if root 'tab-bar-tab 'tab-bar-tab-inactive))
                         pi-deck-root :help "mouse-1: overview of all agents (again: back)")
          items)
    (push `(pi-deck-sep-home menu-item ,(tab-bar-separator) ignore) items)
    (push `(pi-deck-space menu-item
                          ,(pi-deck--tab-bar-button
                            (concat " " (pi-deck--icon-prefix status) (or space "no space") " ▾ ")
                            'tab-bar-tab-group-current)
                          pi-deck-spaces-menu
                          :help "mouse-1: switch space; mouse-3: this space's menu")
          items)
    (dolist (tab tabs)
      (cl-incf index)
      (when (and (not (pi-deck-root-tab-p tab)) (equal (pi-deck--tab-group tab) space))
        (cl-incf number)
        (let ((current (eq (car tab) 'current-tab)))
          (push `(,(intern (format "sep-%d" index)) menu-item ,(tab-bar-separator) ignore) items)
          (push `(,(if current 'current-tab (intern (format "tab-%d" index)))
                  menu-item ,(pi-deck--format-tab-name tab number)
                  ,(if current 'ignore (alist-get 'binding tab))
                  :help ,(format "mouse-1: select; mouse-2 or ✕: close; mouse-3: menu"))
                items))))
    (push `(pi-deck-sep-add menu-item ,(tab-bar-separator) ignore) items)
    (push `(pi-deck-add menu-item ,(pi-deck--tab-bar-button " + " 'tab-bar-tab-inactive)
                        pi-deck-new-tab-menu :help "mouse-1: new agent, shell or empty tab")
          items)
    (nreverse items)))

(defun pi-deck--tab-bar-format ()
  "Return pi-deck's `tab-bar-format'."
  (list #'pi-deck-tab-bar-format-space
        #'tab-bar-format-align-right
        #'pi-deck-tab-bar-format-status))

;;;; Mouse

(defun pi-deck--tab-bar-item (event)
  "Return the tab bar item (KEY BINDING CLOSE-P) clicked in EVENT."
  (tab-bar--event-to-item (event-start event)))

(defun pi-deck--own-item-p (item)
  "Return non-nil when ITEM is one of pi-deck's own tab bar buttons."
  (string-prefix-p "pi-deck-" (symbol-name (or (car item) 'nil))))

(defun pi-deck-tab-bar-mouse-down-1 (event)
  "Select the tab pressed in EVENT; pi-deck's buttons wait for the release."
  (interactive "e")
  (unless (pi-deck--own-item-p (pi-deck--tab-bar-item event))
    (tab-bar-mouse-down-1 event)))

(defun pi-deck-tab-bar-mouse-1 (event)
  "Run pi-deck's button released in EVENT, or close a tab through its ✕."
  (interactive "e")
  (let* ((item (pi-deck--tab-bar-item event))
         (number (tab-bar--key-to-number (car item))))
    (cond
     ((pi-deck--own-item-p item)
      (when (commandp (nth 1 item)) (call-interactively (nth 1 item))))
     ((and (nth 2 item) (not (eq number t)))
      (pi-deck-close-tab (if (integerp number) number nil)))
     (t (tab-bar-mouse-1 event)))))

(defun pi-deck-tab-bar-mouse-2 (event)
  "Close the tab clicked in EVENT Herdr-style."
  (interactive "e")
  (let ((number (tab-bar--key-to-number (car (pi-deck--tab-bar-item event)))))
    (unless (eq number t)
      (pi-deck-close-tab (if (integerp number) number nil)))))

(defun pi-deck-tab-bar-mouse-3 (event)
  "Open the context menu of the tab bar item clicked in EVENT."
  (interactive "e")
  (let* ((item (pi-deck--tab-bar-item event))
         (number (tab-bar--key-to-number (car item))))
    (cond
     ((eq (car item) 'pi-deck-space)
      (if-let* ((space (pi-deck--tab-bar-space (pi-deck--tabs))))
          (pi-deck-popup (pi-deck-space-menu space) event)
        (call-interactively #'pi-deck-spaces-menu)))
     ((not (eq number t))
      (pi-deck-popup (pi-deck-tab-context-menu
                      (if (integerp number) number (1+ (tab-bar--current-tab-index))))
                     event))
     (t (pi-deck-popup (pi-deck--new-tab-menu-map) event)))))

(defconst pi-deck--tab-bar-bindings
  '(("<down-mouse-1>" . pi-deck-tab-bar-mouse-down-1)
    ("<mouse-1>" . pi-deck-tab-bar-mouse-1)
    ("<mouse-2>" . pi-deck-tab-bar-mouse-2)
    ;; Menus open on release: a text terminal menu opened on press picks
    ;; its first item at the release.
    ("<down-mouse-3>" . ignore)
    ("<mouse-3>" . pi-deck-tab-bar-mouse-3))
  "`tab-bar-map' bindings installed by `pi-deck-mode'.")

;;;; Names

(defun pi-deck--tab-main-agent (&optional frame)
  "Return the agent FRAME's current tab is named after, or nil.
That is the first Pi agent in window order (top left first), else the first
terminal; side windows do not count, so the name never depends on the
selected window."
  (let ((agents (delq nil (mapcar (lambda (window)
                                    (unless (window-parameter window 'window-side)
                                      (let ((buffer (window-buffer window)))
                                        (pi-deck-agent (or (pilish-session-chat-buffer buffer)
                                                           buffer)))))
                                  (window-list frame 'no-minibuf (frame-first-window frame))))))
    (or (cl-find 'pi agents :key #'pi-deck-agent-kind) (car agents))))

(defun pi-deck-tab-name ()
  "Name the current tab after its main agent (see `pi-deck--tab-main-agent').
Without agents, the main window's buffer names it.  Explicitly renamed tabs
keep their names."
  (let ((agent (pi-deck--tab-main-agent)))
    (if agent
        (pi-deck-agent-name agent)
      (buffer-name (window-buffer (pi-deck--main-window))))))

;;;; Installing

(defun pi-deck-tab-bar-apply ()
  "Apply pi-deck's tab bar settings."
  (setq tab-bar-format (pi-deck--tab-bar-format)
        tab-bar-tab-name-function #'pi-deck-tab-name)
  (force-mode-line-update t))

(defun pi-deck-tab-bar-enable ()
  "Install pi-deck's tab bar, remembering the previous settings."
  (unless pi-deck--saved-tab-bar-settings
    (setq pi-deck--saved-tab-bar-settings
          (list (cons 'tab-bar-format tab-bar-format)
                (cons 'tab-bar-tab-name-function tab-bar-tab-name-function)
                (cons 'bindings (mapcar (lambda (binding)
                                          (cons (car binding) (keymap-lookup tab-bar-map (car binding))))
                                        pi-deck--tab-bar-bindings)))))
  (dolist (binding pi-deck--tab-bar-bindings)
    (keymap-set tab-bar-map (car binding) (cdr binding)))
  (pi-deck-tab-bar-apply)
  (tab-bar-mode 1))

(defun pi-deck-tab-bar-disable ()
  "Restore the tab bar settings from before pi-deck."
  (dolist (cell pi-deck--saved-tab-bar-settings)
    (if (eq (car cell) 'bindings)
        (dolist (binding (cdr cell))
          (keymap-set tab-bar-map (car binding) (cdr binding)))
      (set (car cell) (cdr cell))))
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

(pi-deck--define-toggle pi-deck-toggle-status-cell pi-deck-tab-bar-status-cell
  "Toggle the tab bar status summary.")
(pi-deck--define-toggle pi-deck-toggle-tab-icons pi-deck-tab-bar-icons
  "Toggle status icons on tabs and the space button.")
(pi-deck--define-toggle pi-deck-toggle-tab-numbers pi-deck-tab-numbers
  "Toggle tab numbers within the space.")

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
