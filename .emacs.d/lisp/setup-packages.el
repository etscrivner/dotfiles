;;; setup-packages.el --- Setup for emacs package system
;;;
;;; Commentary:
;;;     Here we provide additional package archives and automatically install
;;; any missing required packages.
;;;
(require 'package)

;;; Code:

;;; These are the packages we'd like installed by default
(defvar default-package-list
  '(flycheck
    projectile
    helm-projectile
    persp-mode))

;;; Add additional package archives (try to use stable ones)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;; Handle older versions of emacs
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; Initialize the package management system
(package-initialize)

;;; Install any missing packages
(dolist (package default-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'setup-packages)
;;; setup-packages.el ends here
