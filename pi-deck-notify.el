;;; pi-deck-notify.el --- Attention notifications -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Herdr's two notifications: "needs attention" when an agent becomes
;; blocked, and "finished" when it leaves work for idle.  Both are suppressed
;; while the agent is visible.  Desktop notifications use D-Bus when
;; available; otherwise the echo area.

;;; Code:

(require 'pi-deck-core)

(declare-function notifications-notify "notifications" (&rest params))
(declare-function dbus-ping "dbus" (bus service &optional timeout))
(declare-function pi-deck-jump "pi-deck-goto" (agent))

(defcustom pi-deck-notify-method 'auto
  "How to notify about agents.
`auto' uses D-Bus when available, else the echo area; `dbus', `message', or
nil (off) select one explicitly."
  :type '(choice (const auto) (const dbus) (const message) (const :tag "Off" nil))
  :group 'pi-deck)

(defcustom pi-deck-notify-on '(blocked finished)
  "Transitions that notify: `blocked' (needs attention) and `finished'."
  :type '(set (const blocked) (const finished))
  :group 'pi-deck)

(defcustom pi-deck-notify-shells nil
  "Whether shell panes notify; they have no reporting integration."
  :type 'boolean
  :group 'pi-deck)

(defcustom pi-deck-notify-bell nil
  "Whether notifications also ring the bell (useful in terminals)."
  :type 'boolean
  :group 'pi-deck)

(defvar pi-deck-notify-function #'pi-deck-notify-deliver
  "Function called with TITLE, BODY and AGENT to deliver a notification.")

(defvar pi-deck-notify-functions nil
  "Functions called with KIND, TITLE, BODY and AGENT for every notification.
KIND is `blocked' or `finished'.  They run on the same transitions as the
notification itself (`pi-deck-notify-on', `pi-deck-notify-shells', agent
not visible), also when `pi-deck-notify-method' is nil, e.g. to play a
sound or notify another machine.")

(defvar pi-deck--dbus-available 'unknown
  "Cached D-Bus notification service availability.")

(defun pi-deck--dbus-available-p ()
  "Return non-nil when a desktop notification service answers on D-Bus."
  (when (eq pi-deck--dbus-available 'unknown)
    (setq pi-deck--dbus-available
          (and (featurep 'dbusbind)
               (getenv "DBUS_SESSION_BUS_ADDRESS")
               (require 'dbus nil t)
               (ignore-errors
                 (dbus-ping :session "org.freedesktop.Notifications" 500))
               (require 'notifications nil t)
               t)))
  pi-deck--dbus-available)

(defun pi-deck-notify-deliver (title body agent)
  "Deliver a notification with TITLE and BODY about AGENT."
  (when pi-deck-notify-bell (ding t))
  (if (and (memq pi-deck-notify-method '(auto dbus)) (pi-deck--dbus-available-p))
      (notifications-notify
       :title title :body body :app-name "Emacs pi-deck"
       :urgency (if (eq (pi-deck-agent-state agent) 'blocked) 'critical 'normal)
       :actions '("default" "Show")
       :on-action (lambda (_id _key)
                    (when (buffer-live-p (pi-deck-agent-buffer agent))
                      (pi-deck-jump agent))))
    (message "%s: %s" title body)))

(defun pi-deck-notify-transition (agent old new)
  "Notify about AGENT's change from OLD to NEW base state when it matters.
Delivers through `pi-deck-notify-function' unless `pi-deck-notify-method'
is nil, and runs `pi-deck-notify-functions'."
  (when (and (or pi-deck-notify-shells (eq (pi-deck-agent-kind agent) 'pi))
             (not (pi-deck-agent-visible-p agent)))
    (let ((kind (cond ((and (eq new 'blocked) (not (eq old 'blocked))) 'blocked)
                      ;; A worker's result goes to its parent, not to you.
                      ((and (eq new 'idle) (memq old '(working blocked))
                            (not (pi-deck-agent-worker-p agent)))
                       'finished))))
      (when (memq kind pi-deck-notify-on)
        (let ((title (format "%s %s" (pi-deck-agent-name agent)
                             (if (eq kind 'blocked) "needs attention" "finished")))
              (body (or (pi-deck-agent-detail agent)
                        (abbreviate-file-name (pi-deck-agent-directory agent)))))
          (when pi-deck-notify-method
            (funcall pi-deck-notify-function title body agent))
          (run-hook-with-args 'pi-deck-notify-functions kind title body agent))))))

(provide 'pi-deck-notify)
;;; pi-deck-notify.el ends here
