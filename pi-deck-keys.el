;;; pi-deck-keys.el --- Key sheet for the root overview -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; The overview ends with a compact sheet of the keys that matter for
;; agent control.  Keys come from the live keymaps, so the sheet follows
;; rebinding: global commands are looked up in the overview's active maps
;; (the Evil `SPC d' leader when present, else `C-c d'), Pilish commands
;; in Pilish's mode maps, and menu-only commands through the transient
;; menu.  The looked-up entries are cached until a file loads (where
;; keymaps change) or the overview is refreshed explicitly; the laid-out
;; text is cached per window width.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)

(declare-function pi-deck-save-preference "pi-deck-tabbar" (variable))

(defcustom pi-deck-key-sheet t
  "Whether the root overview shows the key sheet.
`pi-deck-key-sheet-toggle' changes it and saves the choice as a preference."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-key-sheet-groups
  '(("Agents"
     ("new agent" pi-deck-new-agent :click t)
     ("new shell" pi-deck-new-shell :click t)
     ("fork agent" pi-deck-fork :click t)
     ("go to agent" pi-deck-goto :click t)
     ("next attention" pi-deck-next-attention :click t)
     ("answer dialog" pi-deck-answer-dialog :click t)
     ("stop at point" pi-deck-overview-stop :maps (pi-deck-overview-mode-map)))
    ("Spaces & tabs"
     ("new space" pi-deck-create-space :click t)
     ("switch space" pi-deck-switch-space :click t)
     ("new tab" pi-deck-new-tab :click t)
     ("tab N" pi-deck-select-tab :range t)
     ("close tab" pi-deck-close-tab)
     ("overview ⇄ back" pi-deck-root :click t))
    ("Panes"
     ("split right" pi-deck-split-right)
     ("split below" pi-deck-split-below)
     ("zoom" pi-deck-zoom)
     ("close pane" pi-deck-close-pane)
     ("repair pairs" pi-deck-repair-tab)
     ("layout preset" pi-deck-apply-layout))
    ("Chat & input"
     ("send" pilish-send :maps (pilish-input-mode-map))
     ("stop turn" pilish-abort :maps (pilish-input-mode-map pilish-chat-mode-map))
     ("model" pilish-select-model :maps (pilish-input-mode-map pilish-chat-mode-map))
     ("thinking" pilish-cycle-thinking :maps (pilish-input-mode-map pilish-chat-mode-map))
     ("steer queue" pilish-queue-steering :maps (pilish-input-mode-map))
     ("Pi menu" pilish-menu :maps (pilish-input-mode-map pilish-chat-mode-map)))
    ("Pi asks"
     ("choose" pilish-dialog-choose :maps (pilish-dialog-widget-map))
     ("choice N" pilish-dialog-choose-digit :maps (pilish-dialog-widget-map) :range t)
     ("next choice" pilish-dialog-next-choice :maps (pilish-dialog-widget-map))
     ("cancel" pilish-dialog-cancel :maps (pilish-dialog-widget-map)))
    ("Windows"
     ("maximize" delete-other-windows)
     ("close window" delete-window)
     ("undo layout" winner-undo))
    ("Daemon"
     ("menu" pi-deck-menu :click t)
     ("sidebar" pi-deck-sidebar-toggle :click t)
     ("reload config" pi-deck-reload-config :click t)
     ("restart daemon" pi-deck-restart-daemon :click t)))
  "Groups of the key sheet: (TITLE ENTRY...).
Each ENTRY is (LABEL COMMAND . PROPERTIES).  The sheet shows COMMAND's
key, or omits the entry when it has none.  PROPERTIES:
  :maps   keymap variables to search instead of the overview's active maps
  :range  show all keys sharing a prefix as one range, e.g. \"SPC d 1…9\"
  :click  clicking the entry runs COMMAND"
  :type '(repeat (cons string (repeat (cons string (cons symbol plist)))))
  :group 'pi-deck)

(defcustom pi-deck-key-sheet-substitutions nil
  "Alist of (COMMAND . REPLACEMENT) for the key sheet.
An entry for COMMAND shows and runs REPLACEMENT when REPLACEMENT has a
key, e.g. ((delete-other-windows . my-toggle-maximize)) when the
configuration binds its own maximize command."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'pi-deck)

(defvar pi-deck-key-sheet--entries 'stale
  "Cached looked-up groups: ((TITLE (KEY LABEL COMMAND CLICK)...)...).")

(defvar pi-deck-key-sheet--layouts nil
  "Alist of WIDTH -> laid-out sheet string for the cached entries.")

(defun pi-deck-key-sheet-invalidate (&rest _)
  "Forget the cached sheet; it is rebuilt on its next display."
  (setq pi-deck-key-sheet--entries 'stale
        pi-deck-key-sheet--layouts nil))

;; Keymaps change when files load (packages, the configuration).
(add-hook 'after-load-functions #'pi-deck-key-sheet-invalidate)

;;;; Lookup

(defun pi-deck-key-sheet--plain-key-p (key)
  "Return non-nil when KEY consists of keyboard events only."
  (cl-every (lambda (event)
              (not (and (symbolp event)
                        (or (memq event '(remap menu-bar tool-bar tab-bar header-line
                                                mode-line left-margin right-margin))
                            (string-suffix-p "-state" (symbol-name event))
                            (string-match-p "mouse\\|wheel" (symbol-name event))))))
            key))

(defun pi-deck-key-sheet--rank (key global)
  "Return KEY's sort rank, lower first.
GLOBAL keys prefer the leader (`SPC'), then `C-c', over a view's own
short keys; keys of a specific keymap prefer the shortest.  Character
events beat function keys (TAB over <tab>), letters beat punctuation."
  (let* ((class (pcase (aref key 0) (32 0) (3 1) (_ 2)))
         (last (aref key (1- (length key))))
         (rest (list (if (cl-every #'characterp key) 0 1)
                     (if (and (characterp last) (<= ?a (downcase last) ?z)) 0 1))))
    (if global
        (cl-list* class (length key) rest)
      (cl-list* (length key) class rest))))

(defun pi-deck-key-sheet--describe (key)
  "Describe KEY, writing C-m after a prefix as C-m rather than RET."
  (mapconcat (lambda (event)
               (if (and (eq event ?\C-m) (> (length key) 1))
                   "C-m"
                 (single-key-description event)))
             key " "))

(defun pi-deck-key-sheet--keys (command maps)
  "Return COMMAND's plain keys in MAPS (keymap variables), best first.
With nil MAPS, the current buffer's active maps are searched."
  (let ((keymaps (and maps (delq nil (mapcar (lambda (var)
                                               (and (boundp var) (keymapp (symbol-value var))
                                                    (symbol-value var)))
                                             maps)))))
    (unless (and maps (null keymaps))
      (sort (cl-remove-if-not #'pi-deck-key-sheet--plain-key-p
                              (where-is-internal command keymaps))
            :key (lambda (key) (pi-deck-key-sheet--rank key (null maps)))
            :lessp #'value<))))

(defun pi-deck-key-sheet--range (keys)
  "Describe KEYS sharing the first key's prefix as \"PREFIX A…B\"."
  (let* ((prefix (seq-subseq (car keys) 0 -1))
         (lasts (sort (mapcar (lambda (key) (aref key (1- (length key))))
                              (cl-remove-if-not
                               (lambda (key) (and (= (length key) (length (car keys)))
                                                  (equal (seq-subseq key 0 -1) prefix)))
                               keys))
                      #'<)))
    (if (cdr lasts)
        (concat (if (length> prefix 0) (concat (pi-deck-key-sheet--describe prefix) " ") "")
                (single-key-description (car lasts)) "…"
                (single-key-description (car (last lasts))))
      (pi-deck-key-sheet--describe (car keys)))))

(defun pi-deck-key-sheet--menu-key (command)
  "Return the key reaching COMMAND through `pi-deck-menu', or nil."
  (when-let* ((menu (car (pi-deck-key-sheet--keys 'pi-deck-menu nil)))
              (suffix (ignore-errors (transient-get-suffix 'pi-deck-menu command)))
              (spec (if (vectorp suffix) (append suffix nil) suffix))
              (key (plist-get (if (keywordp (cadr spec)) (cdr spec) (nthcdr 2 spec)) :key)))
    (concat (pi-deck-key-sheet--describe menu) " " key)))

(defun pi-deck-key-sheet--entry (entry)
  "Return (KEY LABEL COMMAND CLICK) for ENTRY, or nil without a key."
  (pcase-let* ((`(,label ,command . ,props) entry)
               (maps (plist-get props :maps))
               (replacement (alist-get command pi-deck-key-sheet-substitutions))
               (command (if (and replacement (pi-deck-key-sheet--keys replacement maps))
                            replacement
                          command))
               (keys (pi-deck-key-sheet--keys command maps))
               (key (cond ((null keys) (and (null maps) (pi-deck-key-sheet--menu-key command)))
                          ((plist-get props :range) (pi-deck-key-sheet--range keys))
                          (t (pi-deck-key-sheet--describe (car keys))))))
    (when key
      (list key label command (and (plist-get props :click) (commandp command))))))

(defun pi-deck-key-sheet-entries ()
  "Return the sheet's groups with their keys, looked up in the current buffer."
  (when (eq pi-deck-key-sheet--entries 'stale)
    (setq pi-deck-key-sheet--entries
          (delq nil (mapcar (lambda (group)
                              (when-let* ((entries (delq nil (mapcar #'pi-deck-key-sheet--entry
                                                                     (cdr group)))))
                                (cons (car group) entries)))
                            pi-deck-key-sheet-groups))
          pi-deck-key-sheet--layouts nil))
  pi-deck-key-sheet--entries)

;;;; Layout

(defvar-keymap pi-deck-key-sheet-entry-map
  :doc "Mouse binding of clickable key sheet entries."
  "<mouse-1>" #'pi-deck-key-sheet-mouse-run)

(defun pi-deck-key-sheet--block (group)
  "Return GROUP's lines: its title, then one padded line per entry."
  (let ((width (apply #'max (mapcar (lambda (entry) (string-width (car entry))) (cdr group)))))
    (cons (propertize (car group) 'face 'bold)
          (mapcar (pcase-lambda (`(,key ,label ,command ,click))
                    (let ((line (concat (propertize (string-pad key width) 'face 'help-key-binding)
                                        " " label)))
                      (if click
                          (propertize line 'pi-deck-key-command command 'mouse-face 'highlight
                                      'help-echo (format "mouse-1, RET: %s" command)
                                      'keymap pi-deck-key-sheet-entry-map)
                        line)))
                  (cdr group)))))

(defun pi-deck-key-sheet--stack (blocks columns)
  "Stack BLOCKS in order into at most COLUMNS columns of similar height.
Return a list of columns, each a list of lines; blocks within a column
are separated by an empty line."
  (let ((target (ceiling (apply #'+ (mapcar (lambda (block) (1+ (length block))) blocks))
                         columns))
        result)
    (while (progn
             (setq result nil)
             (let (column (height 0))
               (dolist (block blocks)
                 (when (and column (> (+ height (length block)) target))
                   (push column result)
                   (setq column nil height 0))
                 (setq column (append column (and column (list "")) block)
                       height (1+ (length column))))
               (push column result))
             (> (length result) columns))
      (cl-incf target))
    (nreverse result)))

(defun pi-deck-key-sheet--fit (blocks width)
  "Return BLOCKS stacked in the most columns that fit within WIDTH."
  (cl-loop for columns from (length blocks) downto 1
           for stacked = (pi-deck-key-sheet--stack blocks columns)
           when (or (= columns 1)
                    (<= (+ (apply #'+ (mapcar (lambda (column)
                                                (apply #'max (mapcar #'string-width column)))
                                              stacked))
                           (* 3 (1- (length stacked))))
                        width))
           return stacked))

(defun pi-deck-key-sheet-string (width)
  "Return the key sheet laid out in columns within WIDTH, or nil if empty."
  (or (alist-get width pi-deck-key-sheet--layouts)
      (when-let* ((groups (pi-deck-key-sheet-entries)))
        (let* ((columns (pi-deck-key-sheet--fit (mapcar #'pi-deck-key-sheet--block groups) width))
               (widths (mapcar (lambda (column) (apply #'max (mapcar #'string-width column)))
                               columns))
               (lines (cl-loop for i below (apply #'max (mapcar #'length columns))
                               collect (string-trim-right
                                        (mapconcat (lambda (cell)
                                                     (string-pad (or (nth i (car cell)) "") (cdr cell)))
                                                   (cl-mapcar #'cons columns widths)
                                                   "   "))))
               (sheet (concat (propertize "Keys" 'face 'bold)
                              (propertize "  (h hides)" 'face 'shadow) "\n"
                              (string-join lines "\n") "\n")))
          (push (cons width sheet) pi-deck-key-sheet--layouts)
          sheet))))

;;;; Commands

(defun pi-deck-key-sheet-run ()
  "Run the command of the key sheet entry at point."
  (interactive)
  (if-let* ((command (get-text-property (point) 'pi-deck-key-command)))
      (call-interactively command)
    (user-error "No clickable key entry here")))

(defun pi-deck-key-sheet-mouse-run (event)
  "Run the command of the key sheet entry clicked in EVENT."
  (interactive "e")
  (when-let* ((position (posn-point (event-start event)))
              (command (get-text-property position 'pi-deck-key-command
                                          (window-buffer (posn-window (event-start event))))))
    (call-interactively command)))

(provide 'pi-deck-keys)
;;; pi-deck-keys.el ends here
