;;; pi-deck-core.el --- Agent registry and status model -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Agents are Pilish sessions (identified by their chat buffer) and EAT
;; terminal buffers.  Each agent has a base state (idle, working, blocked,
;; unknown) and a seen flag; an idle agent that worked while nobody was
;; looking is "done" until its buffer is shown.  Attention priority is
;; blocked > done > working > idle > unknown, and rolls up to tabs and spaces
;; as a maximum.  This file derives the state; observers subscribe to
;; `pi-deck-agent-change-functions' and `pi-deck-update-hook'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pilish)

(declare-function pi-orchestration-targets "pi-orchestration" ())
(defvar eat-terminal)

(defgroup pi-deck nil
  "Herdr-style spaces, tabs and agent overview for Pi sessions."
  :group 'tools
  :prefix "pi-deck-")

(defcustom pi-deck-status-icons
  '((blocked . "◉") (done . "●") (working . "◐") (idle . "○") (unknown . "·"))
  "Icons shown for each attention status."
  :type '(alist :key-type symbol :value-type string)
  :group 'pi-deck)

(defcustom pi-deck-include-shells t
  "Whether EAT terminal buffers are agents in the overview."
  :type 'boolean
  :group 'pi-deck)

(defface pi-deck-blocked '((t :inherit error :weight bold))
  "Face for blocked agents."
  :group 'pi-deck)
(defface pi-deck-done '((t :inherit success :weight bold))
  "Face for finished, unseen agents."
  :group 'pi-deck)
(defface pi-deck-working '((t :inherit warning))
  "Face for working agents."
  :group 'pi-deck)
(defface pi-deck-idle '((t :inherit shadow))
  "Face for idle agents."
  :group 'pi-deck)
(defface pi-deck-unknown '((t :inherit shadow))
  "Face for agents whose state is unknown."
  :group 'pi-deck)

(defconst pi-deck-statuses '(blocked done working idle unknown)
  "Attention statuses, most urgent first.")

(defun pi-deck-status-priority (status)
  "Return the attention priority of STATUS; higher is more urgent."
  (pcase status ('blocked 4) ('done 3) ('working 2) ('idle 1) (_ 0)))

(defun pi-deck-rollup (statuses)
  "Return the most urgent of STATUSES, or nil when there are none."
  (car (sort (copy-sequence statuses)
             (lambda (a b) (> (pi-deck-status-priority a)
                              (pi-deck-status-priority b))))))

(defun pi-deck-status-face (status)
  "Return the face for STATUS."
  (intern (format "pi-deck-%s" (or status 'unknown))))

(defun pi-deck-status-icon (status)
  "Return STATUS's icon string, propertized with its face."
  (propertize (or (alist-get status pi-deck-status-icons) "·")
              'face (pi-deck-status-face status)))

;;;; Agents

(cl-defstruct (pi-deck-agent (:constructor pi-deck-agent--make)
                             (:copier nil))
  "A Pi session or terminal tracked by pi-deck."
  buffer            ; chat buffer (pi) or terminal buffer (shell); identity
  kind              ; `pi' or `shell'
  (state 'unknown)  ; base state: idle, working, blocked, unknown
  (seen t)          ; nil after finishing work unobserved
  (changed (float-time)) ; time of the last base-state change
  assigned-space    ; space explicitly assigned by pi-deck, or nil
  parent            ; parent agent buffer (orchestration), or nil
  target)           ; orchestration target ID, or nil

(defvar pi-deck--agents (make-hash-table :test 'eq)
  "Tracked agents, keyed by their buffer.")

(defvar pi-deck-agent-change-functions nil
  "Functions called with AGENT, OLD-STATE and NEW-STATE after a state change.")

(defvar pi-deck-update-hook nil
  "Hook run (debounced) after agents, states or seen flags changed.")

(defun pi-deck-agent-status (agent)
  "Return AGENT's attention status: its base state, or `done'."
  (let ((state (pi-deck-agent-state agent)))
    (if (and (eq state 'idle) (not (pi-deck-agent-seen agent))) 'done state)))

(defun pi-deck-agents ()
  "Return all live agents, sorted by buffer name."
  (let (agents)
    (maphash (lambda (buffer agent)
               (when (buffer-live-p buffer) (push agent agents)))
             pi-deck--agents)
    (sort agents :key (lambda (agent) (buffer-name (pi-deck-agent-buffer agent))))))

(defun pi-deck-agent (buffer)
  "Return the agent tracked for BUFFER, or nil."
  (and buffer (gethash buffer pi-deck--agents)))

(defun pi-deck-pi-buffer-p (buffer)
  "Return non-nil when BUFFER is a Pilish chat buffer."
  (and (buffer-live-p buffer)
       (eq (buffer-local-value 'major-mode buffer) 'pilish-chat-mode)))

(defun pi-deck-shell-buffer-p (buffer)
  "Return non-nil when BUFFER is an EAT terminal buffer."
  (and (buffer-live-p buffer)
       (provided-mode-derived-p (buffer-local-value 'major-mode buffer) 'eat-mode)))

(defun pi-deck-agent-directory (agent)
  "Return AGENT's working directory."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (if (eq (pi-deck-agent-kind agent) 'pi)
        (pilish--chat-session-directory buffer)
      (buffer-local-value 'default-directory buffer))))

(defun pi-deck-agent-process (agent)
  "Return AGENT's process, or nil."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (if (eq (pi-deck-agent-kind agent) 'pi)
        (pilish-session-process buffer)
      (get-buffer-process buffer))))

(defun pi-deck-agent-session-name (agent)
  "Return the Pilish named-session suffix of AGENT, or nil."
  (when (eq (pi-deck-agent-kind agent) 'pi)
    (buffer-local-value 'pilish--canonical-session-name (pi-deck-agent-buffer agent))))

(defun pi-deck-agent-name (agent)
  "Return a short display name for AGENT."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (if (eq (pi-deck-agent-kind agent) 'pi)
        (or (let ((name (buffer-local-value 'pilish--session-name buffer)))
              (and (stringp name) (not (string-empty-p name)) name))
            (pi-deck-agent-session-name agent)
            (file-name-nondirectory
             (directory-file-name (pi-deck-agent-directory agent))))
      (string-trim (buffer-name buffer) "\\*" "\\*"))))

(defun pi-deck-agent-model (agent)
  "Return AGENT's model ID, or nil."
  (when (eq (pi-deck-agent-kind agent) 'pi)
    (let ((model (plist-get (buffer-local-value 'pilish--state (pi-deck-agent-buffer agent))
                            :model)))
      (and (listp model) (plist-get model :id)))))

(defun pi-deck-agent-session-file (agent)
  "Return AGENT's Pi session file, or nil."
  (when (eq (pi-deck-agent-kind agent) 'pi)
    (plist-get (buffer-local-value 'pilish--state (pi-deck-agent-buffer agent))
               :session-file)))

(defun pi-deck-agent-dialogs (agent)
  "Return the extension dialogs AGENT awaits the user for."
  (when (eq (pi-deck-agent-kind agent) 'pi)
    (pilish-session-pending-dialogs (pi-deck-agent-buffer agent))))

(defun pi-deck-agent-detail (agent)
  "Return a one-line detail for AGENT: dialog title, exit status, or nil."
  (let ((dialog (car (pi-deck-agent-dialogs agent)))
        (process (pi-deck-agent-process agent)))
    (cond
     (dialog (format "asks: %s" (or (plist-get dialog :title)
                                    (plist-get dialog :method))))
     ((and (eq (pi-deck-agent-kind agent) 'shell)
           (buffer-local-value 'pi-deck--shell-exit (pi-deck-agent-buffer agent)))
      (format "exited %s" (buffer-local-value 'pi-deck--shell-exit
                                              (pi-deck-agent-buffer agent))))
     ((and (processp process) (not (process-live-p process)))
      (format "exited %s" (process-exit-status process)))
     ((not process) "no process"))))

(defun pi-deck-agent-windows-buffers (agent)
  "Return the buffers whose display counts as showing AGENT."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (delq nil (list buffer
                    (and (eq (pi-deck-agent-kind agent) 'pi)
                         (pilish-session-input-buffer buffer))))))

(defun pi-deck-agent-visible-p (agent)
  "Return non-nil when AGENT is shown in a window of a visible frame.
Only the selected tab of a frame has windows, so this means the user can see
it now."
  (cl-some (lambda (buffer)
             (and (buffer-live-p buffer) (get-buffer-window buffer 'visible)))
           (pi-deck-agent-windows-buffers agent)))

;;;; State derivation

(defvar-local pi-deck--shell-phase nil
  "Shell prompt phase from OSC 133 or EAT marks: nil, `prompt' or `running'.")

(defvar-local pi-deck--shell-exit nil
  "Exit status of the terminal's process once it exited, else nil.")

(defun pi-deck--pi-state (buffer)
  "Derive the base state of the Pilish chat BUFFER."
  (let ((process (pilish-session-process buffer)))
    (cond
     ((not (and (processp process) (process-live-p process))) 'unknown)
     ((pilish-session-pending-dialogs buffer) 'blocked)
     ((or (memq (buffer-local-value 'pilish--status buffer)
                '(sending streaming compacting))
          (not (member (buffer-local-value 'pilish--activity-phase buffer)
                       '(nil "idle"))))
      'working)
     (t 'idle))))

(defun pi-deck--shell-state (buffer)
  "Derive the base state of the terminal BUFFER."
  (let ((process (get-buffer-process buffer)))
    (cond
     ((not (and process (process-live-p process))) 'idle)
     (t (pcase (buffer-local-value 'pi-deck--shell-phase buffer)
          ('running 'working)
          ('prompt 'idle)
          (_ 'unknown))))))

(defconst pi-deck--shell-mark-regexp
  "\e]\\(?:133;\\([ACD]\\)\\|51;e;\\([BG]\\)\\)"
  "Shell integration marks: OSC 133 A/C/D and EAT's own B (prompt)/G (command).")

(defun pi-deck--shell-scan (buffer output)
  "Update BUFFER's shell phase from the marks in process OUTPUT."
  (let ((start 0) phase)
    (while (string-match pi-deck--shell-mark-regexp output start)
      (setq start (match-end 0)
            phase (if (member (or (match-string 1 output) (match-string 2 output))
                              '("C" "G"))
                      'running 'prompt)))
    (when (and phase (buffer-live-p buffer)
               (not (eq phase (buffer-local-value 'pi-deck--shell-phase buffer))))
      (with-current-buffer buffer (setq pi-deck--shell-phase phase))
      (pi-deck-refresh-agent buffer))))

(defun pi-deck--watch-shell (buffer)
  "Observe prompt marks in the output of terminal BUFFER's process."
  (when-let* ((process (get-buffer-process buffer)))
    (unless (process-get process 'pi-deck-watched)
      (process-put process 'pi-deck-watched t)
      (add-function :before (process-filter process)
                    (lambda (_process output) (pi-deck--shell-scan buffer output))
                    '((name . pi-deck-shell-marks)))
      (add-function :after (process-sentinel process)
                    (lambda (process _event)
                      (unless (process-live-p process)
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (setq pi-deck--shell-exit (process-exit-status process))))
                        (pi-deck-refresh-agent buffer)))
                    '((name . pi-deck-shell-exit))))))

;;;; Updates

(defvar pi-deck--update-timer nil
  "Pending debounced `pi-deck-update-hook' run.")

(defun pi-deck--schedule-update ()
  "Run `pi-deck-update-hook' soon, coalescing bursts of changes."
  (unless pi-deck--update-timer
    (setq pi-deck--update-timer
          (run-with-timer 0.05 nil #'pi-deck--run-update))))

(defun pi-deck--run-update ()
  "Run `pi-deck-update-hook' now."
  (setq pi-deck--update-timer nil)
  (run-hooks 'pi-deck-update-hook)
  (force-mode-line-update t))

(defun pi-deck--derive (agent)
  "Return AGENT's current base state."
  (let ((buffer (pi-deck-agent-buffer agent)))
    (if (eq (pi-deck-agent-kind agent) 'pi)
        (pi-deck--pi-state buffer)
      (pi-deck--shell-state buffer))))

(defun pi-deck--apply (agent state)
  "Record base STATE for AGENT, running observers on change.
Leaving work (working or blocked to idle) while invisible makes it done."
  (let ((old (pi-deck-agent-state agent)))
    (unless (eq old state)
      (setf (pi-deck-agent-state agent) state
            (pi-deck-agent-changed agent) (float-time))
      (when (and (eq state 'idle) (memq old '(working blocked)))
        (setf (pi-deck-agent-seen agent) (pi-deck-agent-visible-p agent)))
      (run-hook-with-args 'pi-deck-agent-change-functions agent old state)
      (pi-deck--schedule-update))))

(defun pi-deck--track (buffer kind)
  "Return the agent for BUFFER of KIND, creating it if necessary."
  (or (gethash buffer pi-deck--agents)
      (let ((agent (pi-deck-agent--make :buffer buffer :kind kind)))
        (puthash buffer agent pi-deck--agents)
        (setf (pi-deck-agent-state agent) (pi-deck--derive agent))
        (when (eq kind 'shell) (pi-deck--watch-shell buffer))
        (pi-deck--schedule-update)
        agent)))

(defun pi-deck-refresh-agent (buffer)
  "Refresh the state of the agent for BUFFER (chat or terminal)."
  (cond
   ((pi-deck-pi-buffer-p buffer)
    (pi-deck--apply (pi-deck--track buffer 'pi) (pi-deck--pi-state buffer)))
   ((and pi-deck-include-shells (pi-deck-shell-buffer-p buffer))
    (pi-deck--apply (pi-deck--track buffer 'shell) (pi-deck--shell-state buffer)))))

(defun pi-deck--orchestration-links ()
  "Return an alist of (BUFFER . (ID . PARENT-BUFFER)) for orchestration targets."
  (when (fboundp 'pi-orchestration-targets)
    (mapcar (lambda (target)
              (cons (plist-get target :buffer)
                    (cons (plist-get target :id) (plist-get target :parent))))
            (pi-orchestration-targets))))

(defun pi-deck-refresh ()
  "Reconcile all agents with live buffers and refresh their states."
  (interactive)
  (let ((links (pi-deck--orchestration-links)))
    (dolist (buffer (buffer-list))
      (pi-deck-refresh-agent buffer))
    (maphash (lambda (buffer agent)
               (if (or (not (buffer-live-p buffer))
                       (and (eq (pi-deck-agent-kind agent) 'shell)
                            (not pi-deck-include-shells)))
                   (progn (remhash buffer pi-deck--agents)
                          (pi-deck--schedule-update))
                 (when (eq (pi-deck-agent-kind agent) 'shell)
                   (pi-deck--watch-shell buffer))
                 (let ((link (cdr (assq buffer links))))
                   (unless (and (equal (car link) (pi-deck-agent-target agent))
                                (eq (cdr link) (pi-deck-agent-parent agent)))
                     (setf (pi-deck-agent-target agent) (car link)
                           (pi-deck-agent-parent agent) (cdr link))
                     (pi-deck--schedule-update)))))
             pi-deck--agents)))

(defun pi-deck-mark-visible-seen (&rest _)
  "Mark agents shown in a visible frame as seen."
  (maphash (lambda (_buffer agent)
             (when (and (not (pi-deck-agent-seen agent))
                        (pi-deck-agent-visible-p agent))
               (setf (pi-deck-agent-seen agent) t)
               (pi-deck--schedule-update)))
           pi-deck--agents))

(defun pi-deck-mark-seen (agent)
  "Mark AGENT as seen."
  (unless (pi-deck-agent-seen agent)
    (setf (pi-deck-agent-seen agent) t)
    (pi-deck--schedule-update)))

(defun pi-deck--on-activity (chat-buffer _input _old _new _reason)
  "Refresh CHAT-BUFFER's agent after a Pilish activity phase change."
  (when (buffer-live-p chat-buffer)
    (pi-deck-refresh-agent chat-buffer)))

(defun pi-deck--on-dialog (chat-buffer _request _state)
  "Refresh CHAT-BUFFER's agent after an extension dialog opened or closed."
  (when (buffer-live-p chat-buffer)
    (pi-deck-refresh-agent chat-buffer)))

(defun pi-deck--on-target (&rest _)
  "Refresh agents after an orchestration target started or closed."
  (pi-deck-refresh))

(defun pi-deck-summary (&optional agents)
  "Return an alist of (STATUS . COUNT) over AGENTS (default all), urgent first."
  (let ((agents (or agents (pi-deck-agents))))
    (cl-loop for status in pi-deck-statuses
             for count = (cl-count status agents :key #'pi-deck-agent-status)
             when (> count 0) collect (cons status count))))

(defun pi-deck-age (time)
  "Return a short description of how long ago TIME was."
  (let ((seconds (max 0 (- (float-time) time))))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm" (/ seconds 60)))
          ((< seconds 86400) (format "%dh" (/ seconds 3600)))
          (t (format "%dd" (/ seconds 86400))))))

(provide 'pi-deck-core)
;;; pi-deck-core.el ends here
