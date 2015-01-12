;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)

;;;; Functions for creating ndarrays.

(in-package :cl-numerical.ndarray)

(defun ctype-to-lisp-type (ctype)
  (cond
    ((equal ctype :int) 'fixnum)
    ((equal ctype :float) 'single-float)))

(defun coerce-to-cffi-type (val &key (ctype *DEFAULT-CTYPE*))
  (coerce val (ctype-to-lisp-type ctype)))

