;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; Definition of a memory buffer backed by a foreign array in CPU memory.

(in-package :cl-numerical.ndarray)

(defclass c-buffer (buffer)
  ((data  :initarg       :data
          :initform      nil
          :type          :pointer
          :reader        data
          :documentation "Pointer to a C memory buffer"))
  (:documentation "C memory buffer"))

(defmethod initialize-instance ((buffer c-buffer) &key ctype size initial-contents)
  (setf (slot-value buffer 'ctype) ctype)
  (setf (slot-value buffer 'size) size)
  (let ((data (if (null initial-contents)
                  (cffi:foreign-alloc ctype :count size)
                  (if (listp initial-contents)
                      (cffi:foreign-alloc ctype :count size :initial-contents initial-contents)
                      (cffi:foreign-alloc ctype :count size :initial-element initial-contents)))))
    
    (setf (slot-value buffer 'data) data)
        
    (format t "ASAS~%")
    ;; Setup cleanup code
    #+sbcl (sb-ext:finalize buffer (lambda () (format t "Clearing ~%") (cffi:foreign-free data))))
  
  buffer)

(defmethod mref ((buffer c-buffer) index)
  (cffi:mem-aref (data buffer) (ctype buffer) index))

(defmethod set-mref ((buffer c-buffer) index val)
  (setf (cffi:mem-aref (data buffer) (ctype buffer) index) val))

(defsetf mref (buffer index) (val)
  `(set-mref ,buffer ,index ,val))

(defmethod print-object ((buffer c-buffer) stream)
  (format stream "C buffer containing ~a elements of type ~a" (size buffer) (ctype buffer)))
