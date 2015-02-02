;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; Definition of a 1D vector.

(in-package :cl-numerical.ndarray)

(defclass vector ()
  ((memtype :initarg       :memtype
            :initform      (error ":memtype must be specified")
            :type          (member :lisp :c)
            :accessor      memtype
            :documentation "Backing memory type")
   (buffer  :initarg       :buffer
            :initform      (error ":buffer must be specified")
            :type          'buffer
            :accessor      buffer
            :documentation "Vector"))
  (:documentation "1D vector"))

(defun make-vector (&key (memtype *DEFAULT-MEMTYPE*) (ctype :int) size (initial-contents nil))
  (assert (member memtype '(:lisp :c)))
  (make-instance 'vector
                 :memtype memtype
                 :buffer (cond
                           ((equal memtype :lisp) (make-instance 'lisp-buffer
                                                                 :ctype ctype
                                                                 :size size
                                                                 :initial-contents initial-contents))
                           ((equal memtype :c) (make-instance 'c-buffer
                                                              :ctype ctype
                                                              :size size
                                                              :initial-contents initial-contents)))))

(defun make-vector-from-list (list &key (ctype :int))
  (make-vector :memtype *DEFAULT-MEMTYPE*
               :ctype ctype
               :size (length list)
               :initial-contents list))

(defmethod size ((vector vector))
  (size (buffer vector)))

(defmethod vref ((vector vector) index)
  (mref (buffer vector) index))

(defmethod set-vref ((vector vector) index val)
  (setf (mref (buffer vector) index) val))

(defsetf vref (vector index) (val)
  `(set-vref ,vector ,index ,val))

(defmethod print-object ((vector vector) stream)
  (let ((len (if (> (size vector) *MAX-VALS-PRINTED-PER-ROW*)
                 *MAX-VALS-PRINTED-PER-ROW*
                 (size vector))))
    (format stream "[")
    (loop for i from 0 below len do
         (when (/= i 0)
           (format stream ", "))
         (format stream "~a" (vref vector i)))
    (format stream "]")))

(defun vectorp (obj)
  (typep obj 'vector))
