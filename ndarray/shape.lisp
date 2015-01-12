;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)

;;;; Utility function for managing ndarray shapes.

(in-package :cl-numerical.ndarray)

(defun shape-to-size (shape)
  "Compute the number of elements in a given shape (list or array)"
  (let ((size 1))
    (loop for i from 0 below (size shape) do
         (setf size (* size (vref shape i))))
    size))

(defun shape-to-strides (shape)
  "Compute the axes strides for an ndarray with the given shape in contiguous memory"
  (let ((strides (make-c-vector :ctype :int :size (size shape)))
        (prod 1))
    (loop for i from 0 below (size shape) do
         (setf (vref strides i) prod)
         (setf prod (* prod (vref shape i))))
    strides))
                               
(defmethod shape= ((array1 ndarray) (array2 ndarray))
  "Compares two shape vectors to check if they are identical"
  (let ((shape1 (shape array1))
        (shape2 (shape array2)))
    (if (/= (size shape1) (size shape2))
        nil
        (let ((same t))
          (loop for i1 from 0 below (size shape1)
             for i2 from 0 below (size shape2)
             while same do
               (setf same (and same (= (vref shape1 i1) (vref shape2 i2)))))
          same))))
               
(defmethod reshape ((array ndarray) shape)
  "Reshape the ndarray into a new shape"
  (let ((shape-vec (if (c-vector-p shape) shape
                       (make-c-vector :size (length shape)
                                      :ctype :int
                                      :initial-contents (reverse shape)))))
    (if (/= (size array) (shape-to-size shape-vec))
        (error "Size of specified shape does not match that of the array"))
    (let* ((strides-vec (shape-to-strides shape-vec)))
      (make-instance 'ndarray
                     :num-axes (size shape-vec)
                     :shape    shape-vec
                     :strides  strides-vec
                     :offset   (offset array)
                     :buffer   (buffer array)))))
