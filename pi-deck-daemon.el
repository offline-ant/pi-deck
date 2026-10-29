;;; pi-deck-daemon.el --- Emacs server status, config reload and restart -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; `pi-deck-daemon-info' reports the server this Emacs runs (name, socket,
;; PID, clients, frames, uptime) and the Pi orchestration host.  Restarting saves
;; the deck state, starts a detached helper that waits for this process to
;; exit and then runs `pi-deck-restart-command', and exits.  The new daemon
;; restores the saved spaces and sessions like any start of the server
;; owning the state file (see pi-deck-persist).

;;; Code:

(require 'server)
(require 'pi-deck-core)

(declare-function pi-deck--save-now "pi-deck-persist" ())
(defvar pi-deck-state-file)
(defvar pi-deck--state-owner)
(defvar pi-deck--dbus-available)
(defvar pi-deck-notify-method)

(defcustom pi-deck-restart-command '("emacs" "--daemon=%s")
  "Program and arguments that start the replacement daemon.
\"%s\" in an argument is replaced by the running server's absolute socket
file (see `pi-deck-server-socket'), so the replacement listens exactly where
clients connect, whatever its environment would otherwise resolve."
  :type '(repeat string)
  :group 'pi-deck)

(defcustom pi-deck-restart-poll-interval 0.2
  "Seconds between the restart helper's checks that Emacs has exited."
  :type 'number
  :group 'pi-deck)

(defun pi-deck-server-running-p ()
  "Return non-nil when this Emacs runs its server."
  (and (bound-and-true-p server-process) (process-live-p server-process)))

(defun pi-deck-server-socket ()
  "Return this Emacs's server socket file (or TCP auth file), or nil.
An absolute `server-name' is the file itself."
  (when (pi-deck-server-running-p)
    (expand-file-name server-name (if server-use-tcp server-auth-dir server-socket-dir))))

(defun pi-deck-server-display-name ()
  "Return the server name without its directory."
  (file-name-nondirectory server-name))

(defun pi-deck--client-frames ()
  "Return frames that can show spaces: all frames except a daemon's own frame."
  (if (daemonp)
      (cl-remove-if (lambda (frame)
                      (equal (terminal-name (frame-terminal frame)) "initial_terminal"))
                    (frame-list))
    (frame-list)))

(defun pi-deck-daemon-info-lines ()
  "Return lines describing this Emacs server and the Pi integration."
  (let ((sessions (cl-count-if (lambda (agent)
                                 (and (eq (pi-deck-agent-kind agent) 'pi)
                                      (process-live-p (or (pi-deck-agent-process agent) 0))))
                               (pi-deck-agents))))
    (list
     (format "Emacs %s · %s · pid %d · up %s · clients %d · frames %d · live Pi sessions %d"
             emacs-version
             (cond ((and (daemonp) (pi-deck-server-running-p))
                    (format "daemon %S" (pi-deck-server-display-name)))
                   ((pi-deck-server-running-p)
                    (format "server %S" (pi-deck-server-display-name)))
                   (t "no server"))
             (emacs-pid) (emacs-uptime "%h:%.2m")
             (length (bound-and-true-p server-clients))
             (length (pi-deck--client-frames))
             sessions)
     (format "socket %s · orchestration %s · notifications %s"
             (if-let* ((socket (pi-deck-server-socket))) (abbreviate-file-name socket) "-")
             (cond ((not (featurep 'pi-orchestration)) "not loaded")
                   ((pi-deck-server-running-p) "emacs host ready")
                   (t "loaded, needs a server"))
             (pcase pi-deck-notify-method
               ('nil "off")
               ((or 'auto 'dbus) (if (eq pi-deck--dbus-available t) "dbus" "dbus/echo"))
               (_ "echo area")))
     (format "state %s%s" (abbreviate-file-name pi-deck-state-file)
             (if pi-deck--state-owner "" " (not owned: never restored or saved here)")))))

;;;###autoload
(defun pi-deck-daemon-info ()
  "Describe this Emacs server and the Pi integration in a temporary buffer."
  (interactive)
  (with-output-to-temp-buffer "*pi-deck daemon*"
    (princ (string-join (pi-deck-daemon-info-lines) "\n"))))

(defun pi-deck-live-sessions ()
  "Return agents whose processes would die with this Emacs."
  (cl-remove-if-not (lambda (agent) (process-live-p (or (pi-deck-agent-process agent) 0)))
                    (pi-deck-agents)))

;;;###autoload
(defun pi-deck-reload-config ()
  "Reload `user-init-file' in this Emacs."
  (interactive)
  (unless user-init-file (user-error "No init file to reload"))
  (when (or (not (called-interactively-p 'any))
            (yes-or-no-p (format "Reload %s? " (abbreviate-file-name user-init-file))))
    (load user-init-file nil t)
    (message "Reloaded %s" (abbreviate-file-name user-init-file))))

(defun pi-deck--restart-script (pid command)
  "Return a shell script waiting for PID to exit before exec'ing COMMAND."
  (format "while kill -0 %d 2>/dev/null; do sleep %s; done; exec %s"
          pid pi-deck-restart-poll-interval
          (mapconcat #'shell-quote-argument command " ")))

(defun pi-deck-restart-command-line ()
  "Return the expanded restart command."
  (let ((socket (or (pi-deck-server-socket) server-name)))
    (mapcar (lambda (argument)
              (if (string-search "%s" argument) (format argument socket) argument))
            pi-deck-restart-command)))

;;;###autoload
(defun pi-deck-restart-daemon (&optional force)
  "Restart this Emacs daemon, restoring spaces and sessions afterwards.
Lists the live Pi sessions and terminals that will be stopped and asks for
confirmation unless FORCE.  The replacement restores the saved state like
any start (see `pi-deck-auto-restore'): Pi sessions resume from their
session files."
  (interactive)
  (unless (pi-deck-server-running-p)
    (user-error "This Emacs runs no server to restart"))
  (let* ((live (pi-deck-live-sessions))
         (names (mapconcat (lambda (agent)
                             (format "  %s %s (%s)" (pi-deck-agent-kind agent)
                                     (pi-deck-agent-name agent) (pi-deck-agent-status agent)))
                           live "\n")))
    (when (or force
              (yes-or-no-p
               (format "Restart daemon %S?%s\nIt runs: %s\nRestart? "
                       (pi-deck-server-display-name)
                       (if live (format "\nThese %d processes stop now (sessions resume from disk):\n%s"
                                        (length live) names)
                         "")
                       (string-join (pi-deck-restart-command-line) " "))))
      (pi-deck--save-now)
      (call-process "setsid" nil 0 nil "-f" "sh" "-c"
                    (pi-deck--restart-script (emacs-pid) (pi-deck-restart-command-line)))
      (let ((confirm-kill-processes nil)
            (kill-emacs-query-functions nil))
        (kill-emacs)))))

(provide 'pi-deck-daemon)
;;; pi-deck-daemon.el ends here
