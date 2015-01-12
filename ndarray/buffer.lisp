;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; Definition of a C memory buffer.

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
          :documentation "Size of this buffer (in elements)")
   (data  :initarg       :data
          :initform      nil
          :type          :pointer
          :reader        data
          :documentation "Pointer to a C memory buffer"))
  (:documentation "C memory buffer"))

(defmethod initialize-instance ((buffer buffer) &key ctype size initial-contents)
  (setf (slot-value buffer 'ctype) ctype)
  (setf (slot-value buffer 'size) size)
  (setf (slot-value buffer 'data)
        (if (listp initial-contents)
            (cffi:foreign-alloc ctype :count size :initial-contents initial-contents)
            (cffi:foreign-alloc ctype :count size :initial-element initial-contents)))

  ;; Setup cleanup code
  #+sbcl (sb-ext:finalize buffer (lambda () (cffi:foreign-free (data buffer))))
  
  buffer)

(defmethod mref ((buffer buffer) index)
  (cffi:mem-aref (data buffer) (ctype buffer) index))

(defmethod set-mref ((buffer buffer) index val)
  (setf (cffi:mem-aref (data buffer) (ctype buffer) index) val))

(defsetf mref (buffer index) (val)
  `(set-mref ,buffer ,index ,val))

(defmethod print-object ((buffer buffer) stream)
  (format stream "C buffer containing ~a elements of type ~a" (size buffer) (ctype buffer)))
