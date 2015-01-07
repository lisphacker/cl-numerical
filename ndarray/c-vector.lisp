;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; Definition of a 1D vector in foreign memory.

(in-package :cl-numerical.ndarray)

(defclass c-vector (buffer)
  ()
  (:documentation "1D C vector"))

(defun make-c-vector (&key (ctype :int) size (initial-contents nil))
  (make-instance 'c-vector
                 :ctype ctype
                 :size size
                 :initial-contents initial-contents))

(defmethod vref ((vector c-vector) index)
  (mref vector index))

(defmethod set-vref ((vector c-vector) index val)
  (setf (mref vector index) val))

(defsetf vref (c-vector index) (val)
  `(set-vref ,c-vector ,index ,val))

(defmethod print-object ((vector c-vector) stream)
  (let ((len (if (> (size vector) *MAX-VALS-PRINTED-PER-ROW*)
                 *MAX-VALS-PRINTED-PER-ROW*
                 (size vector))))
    (format stream "[")
    (loop for i from 0 below len do
         (when (/= i 0)
           (format stream ", "))
         (format stream "~a" (vref vector i)))
    (format stream "]")))

(defmethod c-vector-p (obj)
  (typep obj 'c-vector))
