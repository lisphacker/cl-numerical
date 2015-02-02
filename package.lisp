;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-USER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)

;;;; Package Definitions.

(in-package :cl-user)

(defpackage :cl-numerical.basic
  (:documentation "Package information"))

(defpackage :cl-numerical.ndarray
  (:documentation "N-Dimensional array")
  (:shadow :vector :vectorp)
  (:use :common-lisp))

(defpackage :cl-numerical
  (:documentation "CL-NUMERICAL system")
  (:use :common-lisp))
