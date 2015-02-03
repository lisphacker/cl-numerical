;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.TESTS -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)

;;;; Tests

(in-package :cl-numerical.tests)

(defun run-tests ()
  (let ((a (arange 0 10)))
    (format t "~a~%" (ndaref a '(1 4)))))
