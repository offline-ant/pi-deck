;;; pi-deck-goto.el --- Agent picker and attention navigation -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `pi-deck-goto' reads an agent with `completing-read', grouped by space
;; (vertico/consult show the groups).  Candidates contain their status word,
;; so typing "blocked" narrows; a prefix argument asks for a status first.
;; Next/previous commands cycle through agents needing attention or all
;; agents in overview order.

;;; Code:

(require 'pi-deck-core)
(require 'pi-deck-spaces)

(defcustom pi-deck-answer-on-jump t
  "Whether jumping to a blocked agent answers its pending dialog."
  :type 'boolean
  :group 'pi-deck)

(defun pi-deck--ordered-agents (&optional tab-index)
  "Return agents in overview order: space order, then tab, then name.
TAB-INDEX caches `pi-deck-tab-index'."
  (let* ((tab-index (or tab-index (pi-deck-tab-index)))
         (spaces (pi-deck-space-names)))
    (sort (pi-deck-agents)
          :key (lambda (agent)
                 (let ((space (pi-deck-agent-space agent tab-index))
                       (tab (pi-deck--agent-tab agent tab-index)))
                   (list (or (cl-position space spaces :test #'equal) most-positive-fixnum)
                         (if tab (nth 1 tab) most-positive-fixnum)
                         (buffer-name (pi-deck-agent-buffer agent))))))))

(defun pi-deck--attention-agents ()
  "Return agents needing attention, most urgent and most recent first."
  (sort (cl-remove-if-not (lambda (agent) (memq (pi-deck-agent-status agent) '(blocked done)))
                          (pi-deck-agents))
        (lambda (a b)
          (let ((pa (pi-deck-status-priority (pi-deck-agent-status a)))
                (pb (pi-deck-status-priority (pi-deck-agent-status b))))
            (if (= pa pb) (> (pi-deck-agent-changed a) (pi-deck-agent-changed b)) (> pa pb))))))

(defun pi-deck-goto-candidates (&optional status)
  "Return an alist of (CANDIDATE . AGENT), optionally only with STATUS."
  (let ((tab-index (pi-deck-tab-index)) candidates)
    (dolist (agent (pi-deck--ordered-agents tab-index))
      (let ((agent-status (pi-deck-agent-status agent)))
        (when (or (null status) (eq status agent-status))
          (let* ((space (or (pi-deck-agent-space agent tab-index) "other"))
                 (tab (pi-deck--agent-tab agent tab-index))
                 (candidate
                  (concat (pi-deck-status-icon agent-status) " "
                          (propertize (format "%-8s" agent-status)
                                      'face (pi-deck-status-face agent-status))
                          " " (pi-deck-agent-name agent)
                          (propertize (format "  %s%s" space
                                              (if tab (concat "/" (pi-deck-tab-label (car tab) (nth 1 tab)))
                                                ""))
                                      'face 'shadow)
                          ;; Keep candidates unique without showing buffer names.
                          (propertize (buffer-name (pi-deck-agent-buffer agent))
                                      'invisible t))))
            (put-text-property 0 1 'pi-deck-space space candidate)
            (push (cons candidate agent) candidates)))))
    (nreverse candidates)))

(defun pi-deck-read-agent (prompt &optional status)
  "Read an agent with PROMPT, grouped by space; optionally only STATUS."
  (let* ((candidates (or (pi-deck-goto-candidates status)
                         (user-error "No %sagents" (if status (format "%s " status) ""))))
         (annotate (lambda (candidate)
                     (when-let* ((agent (cdr (assoc candidate candidates)))
                                 (detail (or (pi-deck-agent-detail agent)
                                             (pi-deck-agent-model agent))))
                       (propertize (concat "  " detail) 'face 'completions-annotations))))
         (group (lambda (candidate transform)
                  (if transform candidate
                    (get-text-property 0 'pi-deck-space candidate))))
         (choice (completing-read
                  prompt
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        `(metadata (category . pi-deck-agent)
                                   (group-function . ,group)
                                   (annotation-function . ,annotate)
                                   (display-sort-function . identity)
                                   (cycle-sort-function . identity))
                      (complete-with-action action candidates string pred)))
                  nil t)))
    (cdr (assoc choice candidates))))

(defun pi-deck-answer-dialog (&optional agent minibuffer)
  "Go to the oldest pending dialog of AGENT (default: the current session).
An inline dialog gets point on its first choice; with MINIBUFFER
\(interactively, a prefix argument) the minibuffer prompts instead."
  (interactive (list nil current-prefix-arg))
  (let ((chat (if agent (pi-deck-agent-buffer agent)
                (or (pilish-session-chat-buffer (window-buffer))
                    (user-error "Not in a Pi session")))))
    (pilish-answer-dialog chat minibuffer)))

(defun pi-deck-jump (agent)
  "Show and focus AGENT; answer its dialog when blocked.
See `pi-deck-answer-on-jump'."
  (pi-deck-reveal agent)
  (when (and pi-deck-answer-on-jump (pi-deck-agent-dialogs agent))
    (pi-deck-answer-dialog agent)))

;;;###autoload
(defun pi-deck-goto (&optional status)
  "Jump to an agent picked by name, grouped by space.
With a prefix argument, first choose a STATUS to narrow to."
  (interactive
   (list (when current-prefix-arg
           (intern (completing-read "Status: " (mapcar #'symbol-name pi-deck-statuses) nil t)))))
  (pi-deck-jump (pi-deck-read-agent "Agent: " status)))

(defun pi-deck--current-agent ()
  "Return the agent shown in the selected window, or nil."
  (let ((buffer (window-buffer)))
    (pi-deck-agent (or (pilish-session-chat-buffer buffer) buffer))))

(defun pi-deck--cycle (agents step)
  "Jump to the agent STEP positions after the current one in AGENTS."
  (unless agents (user-error "No agents"))
  (let* ((position (cl-position (pi-deck--current-agent) agents))
         (next (nth (mod (if position (+ position step) (if (> step 0) 0 -1))
                         (length agents))
                    agents)))
    (pi-deck-jump next)))

;;;###autoload
(defun pi-deck-next-attention ()
  "Jump to the next agent needing attention: blocked first, then done."
  (interactive)
  (let ((agents (pi-deck--attention-agents)))
    (unless agents (user-error "No agent needs attention"))
    (pi-deck-jump (or (cl-find-if-not (lambda (agent) (eq agent (pi-deck--current-agent)))
                                      agents)
                      (car agents)))))

(defun pi-deck-next-agent ()
  "Jump to the next agent in overview order."
  (interactive)
  (pi-deck--cycle (pi-deck--ordered-agents) 1))

(defun pi-deck-previous-agent ()
  "Jump to the previous agent in overview order."
  (interactive)
  (pi-deck--cycle (pi-deck--ordered-agents) -1))

(provide 'pi-deck-goto)
;;; pi-deck-goto.el ends here
