;;; pi-deck.el --- Herdr-style spaces, tabs and agent overview for Pi -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;; Author: Roelof
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (pilish "3.1.0") (ghostel "0.59.0") (transient "0.9.0"))
;; Keywords: tools, processes

;;; Commentary:

;; `pi-deck-mode' turns Emacs into an agent multiplexer for Pi sessions
;; driven through Pilish:
;;
;; - spaces (tab groups per project) holding tabs (window layouts) whose
;;   panes are Pilish chats or ghostel terminals, each with a title bar;
;; - a tab bar showing the current space's tabs, a space menu, the
;;   overview button and a status summary, all usable by mouse;
;; - a sidebar space tree and an overview of every agent with the daemon
;;   status, layout and tab bar controls and a key sheet;
;; - status per agent (blocked, done, working, idle, unknown) with seen
;;   tracking, rolled up to tabs and spaces, a goto picker and
;;   next-attention navigation;
;; - Herdr-style closing: closing a tab, pane or space stops what only it
;;   shows, asking first when that is busy;
;; - notifications when agents need attention or finish;
;; - persistence of spaces, tabs and sessions across daemon restarts.
;;
;; Bind `pi-deck-command-map' to a prefix, e.g. (keymap-global-set "C-c d"
;; pi-deck-command-map).  See README.md.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-workers)
(require 'pi-deck-close)
(require 'pi-deck-headers)
(require 'pi-deck-overview)
(require 'pi-deck-menus)
(require 'pi-deck-goto)
(require 'pi-deck-notify)
(require 'pi-deck-tabbar)
(require 'pi-deck-layout)
(require 'pi-deck-persist)
(require 'pi-deck-daemon)
(require 'pi-deck-menu)

(defcustom pi-deck-refresh-interval 1.0
  "Seconds between reconciliations of agents with live buffers and processes."
  :type 'number
  :group 'pi-deck)

(defcustom pi-deck-dialog-style 'inline
  "How Pi dialogs ask while `pi-deck-mode' is on.
Sets `pilish-extension-dialog-style'.  `inline' (the default) never
interrupts: a blocked agent's question waits in its chat with clickable
choices, focused when jumped to (see `pi-deck-answer-on-jump') or with
`pi-deck-answer-dialog'.  `minibuffer' prompts as soon as Pi asks,
wherever you are typing."
  :type '(choice (const inline) (const minibuffer))
  :group 'pi-deck)

(defvar pi-deck--refresh-timer nil "Reconciliation timer.")
(defvar pi-deck--age-ticks 0 "Reconciliations since the overview ages were redrawn.")
(defvar pi-deck--previous-dialog-style nil "Pilish dialog style before the mode.")
(defvar pi-deck--previous-pairing nil "Whether `pilish-window-pairing-mode' was on before the mode.")
(defvar pi-orchestration-show-session-function)
(declare-function pi-orchestration-show-session "pi-orchestration" (chat))

(defvar-keymap pi-deck-command-map
  :doc "pi-deck commands; bind to a prefix such as \"C-c d\"."
  "d" #'pi-deck-root
  "b" #'pi-deck-sidebar-toggle
  "g" #'pi-deck-goto
  "n" #'pi-deck-next-attention
  "N" #'pi-deck-next-agent
  "P" #'pi-deck-previous-agent
  "a" #'pi-deck-new-agent
  "e" #'pi-deck-new-shell
  "A" #'pi-deck-answer-dialog
  "s" #'pi-deck-switch-space
  "S" #'pi-deck-create-space
  "R" #'pi-deck-rename-space
  "K" #'pi-deck-close-space
  "t" #'pi-deck-new-tab
  "]" #'pi-deck-next-tab
  "[" #'pi-deck-previous-tab
  "r" #'pi-deck-rename-tab
  "X" #'pi-deck-close-tab
  "1" #'pi-deck-select-tab "2" #'pi-deck-select-tab "3" #'pi-deck-select-tab
  "4" #'pi-deck-select-tab "5" #'pi-deck-select-tab "6" #'pi-deck-select-tab
  "7" #'pi-deck-select-tab "8" #'pi-deck-select-tab "9" #'pi-deck-select-tab
  "|" #'pi-deck-split-right
  "-" #'pi-deck-split-below
  "z" #'pi-deck-zoom
  "x" #'pi-deck-close-pane
  "=" #'pi-deck-repair-tab
  "l" #'pi-deck-apply-layout
  "m" #'pi-deck-menu
  "?" #'pi-deck-menu)
(fset 'pi-deck-command-map pi-deck-command-map)

(defun pi-deck--tick ()
  "Reconcile agents; periodically redraw visible views for their ages."
  (pi-deck-refresh)
  (when (>= (cl-incf pi-deck--age-ticks) (max 1 (round (/ 15 pi-deck-refresh-interval))))
    (setq pi-deck--age-ticks 0)
    (pi-deck-overview-refresh-visible)))

(defun pi-deck--on-tab-select (&rest _)
  "Repair pairs; update seen flags, the sidebar and views after a tab switch.
Restored window configurations can lose the pairs' atoms; Pilish repairs
them at the next redisplay, but commands running before it (menu items
that switch tabs and then split) need them now."
  (with-demoted-errors "pi-deck: %S"
    (save-selected-window
      (pi-deck--select-ordinary-window)
      (pilish-repair-window-pairs)))
  (pi-deck--sidebar-sync)
  (pi-deck-mark-visible-seen)
  (pi-deck-overview-refresh-visible))

(defun pi-deck--on-server-frame ()
  "Set up a frame created for an emacsclient."
  (pi-deck-setup-frame (selected-frame)))

(defconst pi-deck--hooks
  '((pilish-activity-phase-functions . pi-deck--on-activity)
    (pilish-extension-dialog-functions . pi-deck--on-dialog)
    (pi-orchestration-target-functions . pi-deck--on-target)
    (pi-deck-agent-change-functions . pi-deck-notify-transition)
    (pi-deck-agent-change-functions . pi-deck--save-on-agent-change)
    (pi-deck-update-hook . pi-deck-overview-refresh-visible)
    (pi-deck-update-hook . pi-deck-headers-update)
    (pi-deck-update-hook . pi-deck--save-on-structure-change)
    (window-configuration-change-hook . pi-deck-schedule-save)
    (kill-buffer-hook . pi-deck--on-kill-buffer)
    (window-buffer-change-functions . pi-deck-mark-visible-seen)
    (window-selection-change-functions . pi-deck-mark-visible-seen)
    (tab-bar-tab-post-select-functions . pi-deck--on-tab-select)
    (tab-bar-tab-post-open-functions . pi-deck--on-tab-select)
    (tab-bar-tab-prevent-close-functions . pi-deck--tab-close-guard)
    (server-after-make-frame-hook . pi-deck--on-server-frame)
    (delete-frame-functions . pi-deck-remember-frame)
    (window-setup-hook . pi-deck-land-on-root)
    (kill-emacs-hook . pi-deck--save-now))
  "Hooks installed by `pi-deck-mode'.")

;;;###autoload
(define-minor-mode pi-deck-mode
  "Global mode providing spaces, tabs, an agent overview and status for Pi."
  :global t
  :group 'pi-deck
  (dolist (hook pi-deck--hooks)
    (if pi-deck-mode
        (add-hook (car hook) (cdr hook))
      (remove-hook (car hook) (cdr hook))))
  (when pi-deck--refresh-timer
    (cancel-timer pi-deck--refresh-timer)
    (setq pi-deck--refresh-timer nil))
  ;; Sessions opened by `/switch emacs' go to the space for their directory.
  (setq pi-orchestration-show-session-function
        (if pi-deck-mode #'pi-deck-show-session #'pi-orchestration-show-session))
  (if pi-deck-mode
      (progn
        (setq pi-deck--previous-dialog-style pilish-extension-dialog-style
              pilish-extension-dialog-style pi-deck-dialog-style
              pi-deck--previous-pairing pilish-window-pairing-mode)
        ;; Panes rely on Pilish keeping each chat over its input.
        (pilish-window-pairing-mode 1)
        (pi-deck-load-preferences)
        (pi-deck-tab-bar-enable)
        (pi-deck-refresh)
        (setq pi-deck--refresh-timer
              (run-with-timer pi-deck-refresh-interval pi-deck-refresh-interval #'pi-deck--tick))
        (pi-deck-restore-if-pending)
        (dolist (frame (pi-deck--client-frames))
          (pi-deck-root-ensure frame)
          (with-selected-frame frame (pi-deck--sidebar-sync))))
    (when pi-deck--save-timer (pi-deck--save-now))
    (setq pilish-extension-dialog-style pi-deck--previous-dialog-style)
    (unless pi-deck--previous-pairing (pilish-window-pairing-mode -1))
    (pi-deck-headers-update)
    (pi-deck-tab-bar-disable)))

(provide 'pi-deck)
;;; pi-deck.el ends here
