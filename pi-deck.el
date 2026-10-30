;;; pi-deck.el --- Spaces and an agent sidebar for Pi -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;; Author: Roelof
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (pilish "3.1.0") (ghostel "0.59.0") (transient "0.9.0"))
;; Keywords: tools, processes

;;; Commentary:

;; `pi-deck-mode' turns Emacs into an agent multiplexer for Pi sessions
;; driven through Pilish:
;;
;; - spaces (one per project) each with one window layout whose panes are
;;   Pilish chats or ghostel terminals, each with a title bar;
;; - a sidebar space tree listing every agent, its workers and panels, with
;;   a status summary, all usable by mouse;
;; - status per agent (blocked, done, working, idle, unknown) with seen
;;   tracking, rolled up to spaces, a goto picker and next-attention
;;   navigation;
;; - hiding panes without stopping agents; stopping agents explicitly,
;;   asking first when that is busy;
;; - notifications when agents need attention or finish;
;; - persistence of spaces, layouts and sessions across daemon restarts.
;;
;; Bind `pi-deck-command-map' to a prefix, e.g. (keymap-global-set "C-c d"
;; pi-deck-command-map).  See README.md.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-workers)
(require 'pi-deck-close)
(require 'pi-deck-headers)
(require 'pi-deck-sidebar)
(require 'pi-deck-menus)
(require 'pi-deck-goto)
(require 'pi-deck-notify)
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
(defvar pi-deck--age-ticks 0 "Reconciliations since the sidebar's ages were redrawn.")
(defvar pi-deck--previous-dialog-style nil "Pilish dialog style before the mode.")
(defvar pi-deck--previous-pairing nil "Whether `pilish-window-pairing-mode' was on before the mode.")
(defvar pi-deck--previous-tab-bar-show nil "`tab-bar-show' before the mode.")
(defvar pi-orchestration-show-session-function)
(declare-function pi-orchestration-show-session "pi-orchestration" (chat))

(defvar-keymap pi-deck-command-map
  :doc "pi-deck commands; bind to a prefix such as \"C-c d\"."
  "b" #'pi-deck-sidebar-toggle
  "g" #'pi-deck-goto
  "n" #'pi-deck-next-attention
  "N" #'pi-deck-next-agent
  "P" #'pi-deck-previous-agent
  "a" #'pi-deck-new-agent
  "e" #'pi-deck-new-shell
  "f" #'pi-deck-fork
  "A" #'pi-deck-answer-dialog
  "s" #'pi-deck-switch-space
  "S" #'pi-deck-create-space
  "R" #'pi-deck-rename-space
  "K" #'pi-deck-close-space
  "|" #'pi-deck-split-right
  "-" #'pi-deck-split-below
  "z" #'pi-deck-zoom
  "x" #'pi-deck-hide-pane
  "=" #'pi-deck-repair-panes
  "l" #'pi-deck-layout-grid
  "m" #'pi-deck-menu
  "?" #'pi-deck-menu)
(fset 'pi-deck-command-map pi-deck-command-map)

(defun pi-deck--tick ()
  "Reconcile agents; periodically redraw the sidebar for its ages."
  (pi-deck-refresh)
  (when (>= (cl-incf pi-deck--age-ticks) (max 1 (round (/ 15 pi-deck-refresh-interval))))
    (setq pi-deck--age-ticks 0)
    (pi-deck-sidebar-refresh-visible)))

(defun pi-deck--on-tab-select (&rest _)
  "Repair pairs and update views after the frame switched space layouts.
Restored window configurations can lose the pairs' atoms; Pilish repairs
them at the next redisplay, but commands running before it (menu items
that switch spaces and then split) need them now."
  (with-demoted-errors "pi-deck: %S"
    (save-selected-window
      (pi-deck--select-ordinary-window)
      (pilish-repair-window-pairs)))
  (pi-deck--on-space-shown))

(defun pi-deck--on-server-frame ()
  "Set up a frame created for an emacsclient."
  (pi-deck-setup-frame (selected-frame)))

(defconst pi-deck--hooks
  '((pilish-activity-phase-functions . pi-deck--on-activity)
    (pilish-extension-dialog-functions . pi-deck--on-dialog)
    (pilish-extension-status-functions . pi-deck--on-extension-status)
    (pi-orchestration-target-functions . pi-deck--on-target)
    (pi-deck-agent-change-functions . pi-deck-notify-transition)
    (pi-deck-agent-change-functions . pi-deck--save-on-agent-change)
    (pi-deck-update-hook . pi-deck-sidebar-refresh-visible)
    (pi-deck-update-hook . pi-deck-headers-update)
    (pi-deck-update-hook . pi-deck--save-on-structure-change)
    (window-configuration-change-hook . pi-deck-schedule-save)
    (kill-buffer-hook . pi-deck--on-kill-buffer)
    (window-buffer-change-functions . pi-deck-mark-visible-seen)
    (window-selection-change-functions . pi-deck-mark-visible-seen)
    (window-size-change-functions . pi-deck--on-window-size-change)
    (tab-bar-tab-post-select-functions . pi-deck--on-tab-select)
    (server-after-make-frame-hook . pi-deck--on-server-frame)
    (delete-frame-functions . pi-deck-remember-frame)
    (window-setup-hook . pi-deck--on-server-frame)
    (kill-emacs-hook . pi-deck--save-now))
  "Hooks installed by `pi-deck-mode'.")

;;;###autoload
(define-minor-mode pi-deck-mode
  "Global mode providing spaces, an agent sidebar and status for Pi.
Each space's layout lives in a hidden tab, so `tab-bar-show' is nil while
the mode is on."
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
              pi-deck--previous-pairing pilish-window-pairing-mode
              pi-deck--previous-tab-bar-show tab-bar-show)
        ;; Panes rely on Pilish keeping each chat over its input.
        (pilish-window-pairing-mode 1)
        (setopt tab-bar-show nil)
        (pi-deck-load-preferences)
        (pi-deck-refresh)
        (setq pi-deck--refresh-timer
              (run-with-timer pi-deck-refresh-interval pi-deck-refresh-interval #'pi-deck--tick))
        ;; A daemon starts its server only after the init file; claim the
        ;; state then, so the owner is known and restored sessions get
        ;; the server's orchestration environment.
        (if (and (daemonp) (not (pi-deck-server-running-p)))
            (add-hook 'emacs-startup-hook #'pi-deck-claim-state)
          (pi-deck-claim-state))
        (mapc #'pi-deck-setup-frame (pi-deck--client-frames)))
    (remove-hook 'emacs-startup-hook #'pi-deck-claim-state)
    (pi-deck-cancel-restore)
    (when pi-deck--save-timer (pi-deck--save-now))
    (setq pilish-extension-dialog-style pi-deck--previous-dialog-style)
    (unless pi-deck--previous-pairing (pilish-window-pairing-mode -1))
    (setopt tab-bar-show pi-deck--previous-tab-bar-show)
    (pi-deck-headers-update)))

(provide 'pi-deck)
;;; pi-deck.el ends here
