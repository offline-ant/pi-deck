;;; pi-deck-menu.el --- Transient menu -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; One menu for spaces, agents, panes, state and the daemon.

;;; Code:

(require 'transient)
(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-close)
(require 'pi-deck-sidebar)
(require 'pi-deck-goto)
(require 'pi-deck-layout)
(require 'pi-deck-persist)
(require 'pi-deck-daemon)

;;;###autoload (autoload 'pi-deck-menu "pi-deck-menu" nil t)
(transient-define-prefix pi-deck-menu ()
  "pi-deck: spaces, agents, panes, state and the daemon."
  [["Spaces"
    ("+" "create" pi-deck-create-space)
    ("s" "switch" pi-deck-switch-space)
    ("R" "rename" pi-deck-rename-space)
    ("K" "close (stops its agents)" pi-deck-close-space)]
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
    ("x" "hide pane" pi-deck-hide-pane)
    ("=" "repair panes" pi-deck-repair-panes)
    ("l" "grid of the space's agents" pi-deck-layout-grid)]]
  [["State"
    ("S" "save" pi-deck-save-state)
    ("L" "restore" pi-deck-restore-state)]
   ["Daemon"
    ("i" "daemon info" pi-deck-daemon-info)
    ("c" "reload config" pi-deck-reload-config)
    ("D" "restart daemon" pi-deck-restart-daemon)]])

(provide 'pi-deck-menu)
;;; pi-deck-menu.el ends here
