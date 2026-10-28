;;; pi-deck.el --- Herdr-style spaces, tabs and agent overview for Pi -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;; Author: Roelof
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (pilish "3.1.0") (eat "0.9.4") (transient "0.9.0"))
;; Keywords: tools, processes

;;; Commentary:

;; `pi-deck-mode' turns Emacs into an agent multiplexer for Pi sessions
;; driven through Pilish:
;;
;; - spaces (tab groups per project) holding tabs (window layouts) whose
;;   panes are Pilish chats or EAT terminals;
;; - a pinned root tab with an overview of every agent, the daemon status
;;   and layout/tab bar controls;
;; - status per agent (blocked, done, working, idle, unknown) with seen
;;   tracking, rolled up to tabs and spaces, a tab bar status cell, a
;;   sidebar, a goto picker and next-attention navigation;
;; - notifications when agents need attention or finish;
;; - persistence of spaces, tabs and sessions across daemon restarts.
;;
;; Bind `pi-deck-command-map' to a prefix, e.g. (keymap-global-set "C-c d"
;; pi-deck-command-map).  See README.md.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-overview)
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

(defcustom pi-deck-defer-dialogs t
  "Whether Pi dialogs wait for the user instead of prompting immediately.
Blocked agents are answered when jumped to (see `pi-deck-answer-on-jump')
or with `pi-deck-answer-dialog'.  Sets `pilish-defer-extension-dialogs'."
  :type 'boolean
  :group 'pi-deck)

(defvar pi-deck--refresh-timer nil "Reconciliation timer.")
(defvar pi-deck--autosave-timer nil "Autosave timer.")
(defvar pi-deck--age-ticks 0 "Reconciliations since the overview ages were redrawn.")
(defvar pi-deck--previous-defer nil "Pilish dialog deferral before the mode.")

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

(defun pi-deck--autosave ()
  "Save the state when there is anything to save."
  (when (or pi-deck-spaces (> (hash-table-count pi-deck--agents) 0))
    (with-demoted-errors "pi-deck autosave: %S" (pi-deck-save-state))))

(defun pi-deck--on-tab-select (&rest _)
  "Update seen flags and the sidebar after a tab switch."
  (pi-deck--sidebar-sync)
  (pi-deck-mark-visible-seen))

(defun pi-deck--on-server-frame ()
  "Set up a frame created for an emacsclient."
  (pi-deck-setup-frame (selected-frame)))

(defconst pi-deck--hooks
  '((pilish-activity-phase-functions . pi-deck--on-activity)
    (pilish-extension-dialog-functions . pi-deck--on-dialog)
    (pi-orchestration-target-functions . pi-deck--on-target)
    (pi-deck-agent-change-functions . pi-deck-notify-transition)
    (pi-deck-update-hook . pi-deck-overview-refresh-visible)
    (window-buffer-change-functions . pi-deck-mark-visible-seen)
    (window-selection-change-functions . pi-deck-mark-visible-seen)
    (tab-bar-tab-post-select-functions . pi-deck--on-tab-select)
    (tab-bar-tab-prevent-close-functions . pi-deck--prevent-root-close)
    (server-after-make-frame-hook . pi-deck--on-server-frame)
    (delete-frame-functions . pi-deck-remember-frame)
    (kill-emacs-hook . pi-deck--autosave))
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
  (dolist (timer '(pi-deck--refresh-timer pi-deck--autosave-timer))
    (when (symbol-value timer) (cancel-timer (symbol-value timer)) (set timer nil)))
  (if pi-deck-mode
      (progn
        (setq pi-deck--previous-defer pilish-defer-extension-dialogs
              pilish-defer-extension-dialogs pi-deck-defer-dialogs)
        (pi-deck-load-preferences)
        (pi-deck-tab-bar-enable)
        (pi-deck-refresh)
        (setq pi-deck--refresh-timer
              (run-with-timer pi-deck-refresh-interval pi-deck-refresh-interval #'pi-deck--tick))
        (when pi-deck-autosave-interval
          (setq pi-deck--autosave-timer
                (run-with-timer pi-deck-autosave-interval pi-deck-autosave-interval
                                #'pi-deck--autosave)))
        (pi-deck-restore-if-pending)
        (dolist (frame (pi-deck--client-frames))
          (pi-deck-root-ensure frame)))
    (setq pilish-defer-extension-dialogs pi-deck--previous-defer)
    (pi-deck-tab-bar-disable)))

(provide 'pi-deck)
;;; pi-deck.el ends here
