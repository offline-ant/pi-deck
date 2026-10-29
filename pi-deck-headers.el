;;; pi-deck-headers.el --- Pane title bars -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Every pane showing an agent (a Pilish chat or a ghostel terminal) gets a
;; title bar in its header line:
;;
;;   ◐ main · claude-opus-5-5 · 41%                      [⑂] [⤢] [✕]
;;
;; status icon, name, then the model and context use for Pi agents or the
;; exit status for terminals; [⑂] forks a Pi session (`pi-deck-fork-agent'),
;; [⤢] zooms the pane and [✕] closes it
;; Herdr-style (see `pi-deck-close-pane').  The segment is put in front of
;; whatever header line the buffer already has; Pilish's input buffers keep
;; their own header line.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)
(require 'pi-deck-close)
(require 'pi-deck-menus)

(defcustom pi-deck-pane-headers t
  "Whether agent panes get pi-deck title bars in their header lines."
  :type 'boolean
  :group 'pi-deck)

(defvar pi-deck-pane-header-prefix-function nil
  "Function returning a string put first in a pane title bar, or nil.
It is called with the pane's window selected, e.g. to show a window
number; nil adds nothing.")

(defconst pi-deck--header-construct '(:eval (pi-deck-pane-header))
  "Header line construct showing the pane title bar.")

(defvar-local pi-deck--saved-header-line nil
  "Header line format of the buffer before pi-deck added its title bar.")

(defun pi-deck--header-window (event)
  "Return the window whose header line EVENT clicked."
  (posn-window (event-start event)))

(defun pi-deck-header-zoom (event)
  "Zoom the pane whose title bar EVENT clicked."
  (interactive "e")
  (select-window (pi-deck--header-window event))
  (pi-deck-zoom))

(defun pi-deck-header-fork (event)
  "Fork the Pi agent whose title bar EVENT clicked; see `pi-deck-fork-agent'."
  (interactive "e")
  (with-current-buffer (window-buffer (pi-deck--header-window event))
    (pi-deck-fork-agent (pi-deck-agent (current-buffer)))))

(defun pi-deck-header-close (event)
  "Close the pane whose title bar EVENT clicked; see `pi-deck-close-pane'."
  (interactive "e")
  (pi-deck-close-pane (pi-deck--header-window event) event))

(defun pi-deck--header-button (label command help)
  "Return header line button LABEL running COMMAND, with HELP."
  (propertize label
              'face 'header-line-highlight
              'mouse-face 'mode-line-highlight 'pointer 'hand 'help-echo help
              'local-map (define-keymap
                        "<header-line> <mouse-1>" command
                        "<header-line> <down-mouse-1>" #'ignore)))

(defun pi-deck--context-percent (agent)
  "Return AGENT's context use as \"41%\", or nil when unknown."
  (let ((percent (plist-get (pilish-session-context-usage (pi-deck-agent-buffer agent))
                            :percent)))
    (and (numberp percent) (format "%d%%" (round percent)))))

(defun pi-deck--header-details (agent)
  "Return AGENT's title bar details: model and context, or exit status."
  (delq nil (append (when (eq (pi-deck-agent-kind agent) 'pi)
                      (list (pi-deck-agent-model agent) (pi-deck--context-percent agent)))
                    (list (pi-deck-agent-detail agent)))))

(defun pi-deck--header-fit (title reserved)
  "Truncate TITLE to leave RESERVED columns of the header line's window.
The header line is formatted with its window selected, so the buttons
after TITLE stay visible however long the name and details are."
  (truncate-string-to-width title (max 4 (- (window-total-width) reserved 1)) nil nil "…"))

(defun pi-deck-pane-header ()
  "Return the title bar of the current buffer's agent pane, or nil."
  (when-let* ((agent (and (bound-and-true-p pi-deck-mode) (pi-deck-agent (current-buffer)))))
    (let ((prefix (or (and pi-deck-pane-header-prefix-function
                           (funcall pi-deck-pane-header-prefix-function))
                      ""))
          (buttons (concat (if (eq (pi-deck-agent-kind agent) 'pi)
                               (concat (pi-deck--header-button "[⑂]" #'pi-deck-header-fork
                                                               "mouse-1: fork this session (new tab)")
                                       " ")
                             "")
                           (pi-deck--header-button "[⤢]" #'pi-deck-header-zoom
                                                   "mouse-1: zoom this pane (again: restore)")
                           " "
                           (pi-deck--header-button "[✕]" #'pi-deck-header-close
                                                   "mouse-1: close this pane; stops its agent unless shown elsewhere")
                           " ")))
      (string-replace
       "%" "%%"
       (concat (pi-deck--header-fit
                (concat prefix " " (pi-deck-status-icon (pi-deck-agent-status agent)) " "
                        (propertize (pi-deck-agent-name agent) 'face 'bold)
                        (mapconcat (lambda (detail) (propertize (concat " · " detail) 'face 'shadow))
                                   (pi-deck--header-details agent) ""))
                (string-width buttons))
               (propertize " " 'display `(space :align-to (- right ,(string-width buttons))))
               buttons
               ;; Pilish's own header line, if any, follows on the same line.
               (if pi-deck--saved-header-line " │ " ""))))))

(defun pi-deck--install-header (buffer)
  "Put the pane title bar in front of BUFFER's header line."
  (when (and pi-deck-pane-headers (buffer-live-p buffer))
    (with-current-buffer buffer
      (unless (and (consp header-line-format)
                   (eq (car header-line-format) pi-deck--header-construct))
        (setq pi-deck--saved-header-line header-line-format)
        (setq-local header-line-format
                    (if header-line-format
                        (list pi-deck--header-construct header-line-format)
                      (list pi-deck--header-construct)))))))

(defun pi-deck--remove-header (buffer)
  "Restore BUFFER's header line from before the pane title bar."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (consp header-line-format)
                 (eq (car header-line-format) pi-deck--header-construct))
        (setq-local header-line-format pi-deck--saved-header-line)
        (kill-local-variable 'pi-deck--saved-header-line)))))

(defun pi-deck-headers-update ()
  "Give every agent buffer its title bar, or remove them all when disabled."
  (maphash (lambda (buffer _agent)
             (if (and pi-deck-pane-headers (bound-and-true-p pi-deck-mode))
                 (pi-deck--install-header buffer)
               (pi-deck--remove-header buffer)))
           pi-deck--agents))

(provide 'pi-deck-headers)
;;; pi-deck-headers.el ends here
