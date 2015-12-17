;;; init.el --- The emacs configuration entry-point
;;;
;;; Commentary:
;;;     This is the first file executed when emacs is initialized.
;;;

;;; Code:
;; Determine our dotfiles directory
(defun get-dotfiles-directory ()
  "Returns the directory the Emacs dotfiles are in."
  (file-name-directory (or (buffer-file-name) load-file-name)))

;; Helper method to be used to add other directories to load path
(defun add-recursively-to-load-path (root-dir dotfiles-dir)
  "Add the given ROOT-DIR and all of its subdirectories to the
  load path. ROOT-DIR should be a subdirectory of the
  DOTFILES-DIR."
  (let ((expanded-dir (expand-file-name root-dir dotfiles-dir)))
    (add-to-list 'load-path expanded-dir)
    (dolist (project (directory-files expanded-dir t "\\w+"))
      (when (file-directory-p project)
        (add-to-list 'load-path project)))))

;; Add third-party emacs packages and our own custom lisp directories
(let ((dotfiles-dir (get-dotfiles-directory)))
  (add-recursively-to-load-path "third-party" dotfiles-dir)
  (add-recursively-to-load-path "lisp" dotfiles-dir))

;; Restclient mode
(require 'restclient)

;; Kick-off overall setup
(require 'setup-packages)
(require 'setup-emacs)

;;; init.el ends here
