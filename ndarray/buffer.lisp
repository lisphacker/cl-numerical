;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; Definition of an abstract memory buffer.

(in-package :cl-numerical.ndarray)

(defclass buffer ()
  ((ctype :initarg       :ctype
          :initform      (error ":ctype must be specified")
          :type          'symbol
          :reader        ctype
          :documentation "CFFI type")
   (size  :initarg       :size
          :initform      (error ":size must be specified")
          :type          'integer
          :reader        size
          :documentation "Size of this buffer (in elements)"))
  (:documentation "Abstract memory buffer type"))

(defmethod initialize-instance ((buffer buffer) &key ctype size initial-contents)
  (error "Cannot initialize ~a directly, use ~a or ~a" 'buffer 'lisp-buffer 'c-buffer))
