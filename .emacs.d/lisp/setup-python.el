;;; setup-python.el --- Python-specific configurations
;;;
;;; Commentary:
;;;     Contains various python-specific configurations
;;;

;;; Code:

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
  (let ((module-name (skeleton-read " Module name:")))
    (concat
     module-name "\n"
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
