;;; install-deps.el --- Install pi-deck test dependencies -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch helper: installs Pilish's and pi-deck's package dependencies into
;; `package-user-dir' (set by the Makefile to a project-local directory).

;;; Code:

(require 'package)
(let ((dir (getenv "PACKAGE_USER_DIR")))
  (when dir (setq package-user-dir (expand-file-name dir))))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(defvar pi-deck-deps '(eat magit-section md-ts-mode markdown-table-wrap))
(unless (cl-every #'package-installed-p pi-deck-deps)
  (package-refresh-contents)
  (dolist (pkg pi-deck-deps)
    (unless (package-installed-p pkg)
      (package-install pkg))))

;;; install-deps.el ends here
