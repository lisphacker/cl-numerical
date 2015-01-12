;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)

;;;; Functions for creating ndarrays.

(in-package :cl-numerical.ndarray)

(defun empty (dimensions &key (ctype *DEFAULT-CTYPE*))
  "Creates an empty ndarray."
  (make-ndarray dimensions :ctype ctype))
;;(export "empty")

(defun zeros (dimensions &key (ctype *DEFAULT-CTYPE*))
  "Creates an ndarray filled with zeros."
  (make-ndarray dimensions :ctype ctype :initial-contents 0.0))

(defun ones (dimensions &key (ctype *DEFAULT-CTYPE*))
  "Creates an ndarray filled with ones."
  (make-ndarray dimensions :ctype ctype :initial-contents 1.0))

(defun arange (start stop &key (step 1) (ctype *DEFAULT-CTYPE*))
  "Creates a 1D array containing evenly spaced values within a given interval"
  (let* ((size  (ceiling (abs (- stop start)) step))
         (array (empty size :ctype ctype)))
    (loop for val from start below stop by step
       for idx from 0 do
         (setf (mref (buffer array) idx) (coerce-to-cffi-type val :ctype ctype)))
    array))

(defun linspace (start stop num &key (ctype *DEFAULT-CTYPE*))
  "Creates a 1D array partitioning an interval into the specified number of partitions"
  (let* ((start2 (coerce-to-cffi-type start :ctype ctype))
         (stop2  (coerce-to-cffi-type stop :ctype ctype))
         (step   (/ (abs (- stop2 start2)) (coerce-to-cffi-type (1- num) :ctype ctype)))
         (array (empty num :ctype ctype)))
    (loop for val from start2 to stop2 by step
       for idx from 0 do
         (setf (mref (buffer array) idx) (coerce-to-cffi-type val :ctype ctype)))))
