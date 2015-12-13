;;; setup-python.el --- Python-specific configurations
;;;
;;; Commentary:
;;;     Contains various python-specific configurations
;;;
(require 'python)

;;; Code:

;;; Docstring fill style
(setq python-fill-docstring-style 'pep-257-nn)

;;; Use ipython, when available, instead of python
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;;; Python helper function
(defun str-replace (str match replace)
  "Replace STR occurences of MATCH with REPLACE."
  (mapconcat 'identity (split-string str match) replace))

(defun convert-path-to-python-module (path module-root)
  "Convert filesystem PATH to a python module name, starting at MODULE-ROOT."
  (let ((path-parts (split-string path (concat module-root "/"))))
    (if (> (length path-parts) 1)
        (str-replace
         (str-replace (cadr path-parts) "/" ".") ".py" "")
      "")))

;;; Code generation for basic python file boiler-plate
(define-skeleton python-skeleton--test-file
  "Inserts boilerplate code for a Python unit-test."
  "Does the following:"
  "# -*- coding: utf-8 -*-\n"
  "import unittest\n"
  "\n"
  "\n"
  "class Test" (skeleton-read "Test name:") "(unittest.TestCase):\n"
  "    ")

(define-skeleton python-skeleton--module-file
  "Inserts boilerplate code for a python module file"
  "Does the following:"
  "# -*- coding: utf-8 -*-\n"
  "\"\"\"\n"
  "    "
  (let ((module-name (skeleton-read " Module name:")))
    (concat
     module-name "\n"
     "    " (make-string (string-width module-name) ?~)))
  "\n"
  "    MODULE DESCRIPTION\n"
  "\n"
  "\n"
  "\"\"\"\n")

;; Employer-specific code generation
(define-skeleton python-skeleton--parkme-module-file
  "Inserts boilerplate code for a python module file"
  "Does the following:"
  "# -*- coding: utf-8 -*-\n"
  "\"\"\"\n"
  "    "
  (let* ((default-module-name (convert-path-to-python-module (buffer-file-name) "realtimeparking"))
         (raw-module-name (skeleton-read (concat " Module name (Default " default-module-name  "):")))
         (module-name (if (string= "" raw-module-name) default-module-name raw-module-name)))
    (concat
     module-name
     "\n"
     "    " (make-string (string-width module-name) ?~)))
  "\n"
  "    MODULE DESCRIPTION\n"
  "\n"
  "    Copyright (C) "
  (format-time-string "%Y")
  " ParkMe Inc. All Rights Reserved\n"
  "\"\"\"\n")

(provide 'setup-python)
;;; setup-python.el ends here
