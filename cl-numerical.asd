;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-USER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)

;;;; ASDF System Definitions.

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))

;;; Basic
(asdf:defsystem :cl-numerical.basic
  :description "Package information"
  :components ((:file "package")))

;;; NDArray
(asdf:defsystem :cl-numerical.ndarray
  :description "N-Dimensional array"
  :depends-on (:cl-numerical.basic :cffi)
  :components ((:module "ndarray"
                        :components (
                                     (:file "buffer")
                                     (:file "constants")
                                     (:file "cffi-util")
                                     (:file "c-vector" :depends-on ("buffer" "constants"))
                                     (:file "ndarray" :depends-on ("buffer" "c-vector" "constants"))
                                     (:file "shape" :depends-on ("c-vector" "ndarray"))
                                     (:file "ndaref" :depends-on ("ndarray"))
                                     (:file "create" :depends-on ("ndarray" "ndaref" "cffi-util"))
                                     
                                     ))))

;;; Main system
(asdf:defsystem :cl-numerical
  :description "cl-numerical system"
  :depends-on (:cl-numerical.basic :cl-numerical.ndarray))

