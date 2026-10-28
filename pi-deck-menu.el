;;; pi-deck-menu.el --- Transient menu -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; One menu for spaces, tabs, agents, layouts, tab bar preferences, state
;; and the daemon.

;;; Code:

(require 'transient)
(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-overview)
(require 'pi-deck-goto)
(require 'pi-deck-layout)
(require 'pi-deck-tabbar)
(require 'pi-deck-persist)
(require 'pi-deck-daemon)

(defun pi-deck-menu--toggle-description (label variable)
  "Return LABEL with VARIABLE's on/off state."
  (format "%s [%s]" label (if (symbol-value variable) "on" "off")))

;;;###autoload (autoload 'pi-deck-menu "pi-deck-menu" nil t)
(transient-define-prefix pi-deck-menu ()
  "pi-deck: spaces, tabs, agents, layouts and the daemon."
  [["Spaces"
    ("+" "create" pi-deck-create-space)
    ("s" "switch" pi-deck-switch-space)
    ("R" "rename" pi-deck-rename-space)
    ("K" "close" pi-deck-close-space)
    ("d" "root/back" pi-deck-root)]
   ["Tabs"
    ("t" "new" pi-deck-new-tab)
    ("]" "next" pi-deck-next-tab :transient t)
    ("[" "previous" pi-deck-previous-tab :transient t)
    ("r" "rename" pi-deck-rename-tab)
    ("X" "close" pi-deck-close-tab)]
   ["Agents"
    ("a" "new Pi agent" pi-deck-new-agent)
    ("e" "new shell" pi-deck-new-shell)
    ("g" "go to…" pi-deck-goto)
    ("n" "next attention" pi-deck-next-attention)
    ("A" "answer dialog" pi-deck-answer-dialog)
    ("b" "sidebar" pi-deck-sidebar-toggle)]
   ["Panes"
    ("|" "split right" pi-deck-split-right)
    ("-" "split below" pi-deck-split-below)
    ("z" "zoom" pi-deck-zoom)
    ("x" "close pane" pi-deck-close-pane)
    ("l" "layout preset" pi-deck-apply-layout)]]
  [["Tab bar"
    ("G" (lambda () (pi-deck-menu--toggle-description "groups" 'pi-deck-tab-bar-groups))
     pi-deck-toggle-tab-groups :transient t)
    ("I" (lambda () (pi-deck-menu--toggle-description "other spaces' tabs"
                                                      'tab-bar-show-inactive-group-tabs))
     pi-deck-toggle-inactive-group-tabs :transient t)
    ("H" (lambda () (pi-deck-menu--toggle-description "numbers" 'tab-bar-tab-hints))
     pi-deck-toggle-tab-hints :transient t)
    ("C" (lambda () (pi-deck-menu--toggle-description "status cell" 'pi-deck-tab-bar-status-cell))
     pi-deck-toggle-status-cell :transient t)
    ("O" (lambda () (pi-deck-menu--toggle-description "icons" 'pi-deck-tab-bar-icons))
     pi-deck-toggle-tab-icons :transient t)
    ("B" (lambda () (pi-deck-menu--toggle-description "show tab bar" 'tab-bar-show))
     pi-deck-toggle-tab-bar :transient t)
    ("W" "save preferences" pi-deck-save-preferences)]
   ["State"
    ("S" "save" pi-deck-save-state)
    ("L" "restore" pi-deck-restore-state)]
   ["Daemon"
    ("c" "reload config" pi-deck-reload-config)
    ("D" "restart daemon" pi-deck-restart-daemon)]])

(provide 'pi-deck-menu)
;;; pi-deck-menu.el ends here
