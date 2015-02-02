;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; Definition of a memory buffer backed by a lisp array.

(in-package :cl-numerical.ndarray)

(defclass lisp-buffer (buffer)
  ((data  :initarg       :data
          :initform      nil
          :type          'array
          :reader        data
          :documentation "Pointer to a C memory buffer"))
  (:documentation "Lisp memory buffer"))

(defmethod initialize-instance ((buffer lisp-buffer) &key ctype size initial-contents)
  (setf (slot-value buffer 'ctype) ctype)
  (setf (slot-value buffer 'size) size)
  (setf (slot-value buffer 'data)
        (if (null initial-contents)
            (make-array size
                        :element-type (ctype-to-lisp-type ctype))
            (if (listp initial-contents)
                (make-array size
                            :element-type (ctype-to-lisp-type ctype)
                            :initial-contents initial-contents)
                (make-array size
                            :element-type (ctype-to-lisp-type ctype)
                            :initial-element initial-contents))))
  buffer)

(defmethod mref ((buffer lisp-buffer) index)
  (aref (data buffer) index))

(defmethod set-mref ((buffer lisp-buffer) index val)
  (setf (aref (data buffer) index) val))

(defsetf mref (buffer index) (val)
  `(set-mref ,buffer ,index ,val))

(defmethod print-object ((buffer lisp-buffer) stream)
  (format stream "Lisp buffer containing ~a elements of type ~a" (size buffer) (ctype buffer)))
