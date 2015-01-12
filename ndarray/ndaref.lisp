;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)

;;;; Functions for referencing ndarray elements.

(in-package :cl-numerical.ndarray)

#|
(defmacro ndaref-list (array indices)
  "Reference an element in the array with the full set of indices, which must be a list of fixnums"
  `(aref (data (buffer ,array)) (+ (offset ,array)
                                   (loop for index in ,indices
                                      for stride across (strides ,array)
                                      summing (* index stride)))))

(defmacro ndaref-vector (array indices)
  "Reference an element in the array with the full set of indices, which must be a list of fixnums"
  (aref (data (buffer array)) (+ (offset array)
                                 (loop for index across indices
                                    for stride across (strides array)
                                    summing (* index stride)))))
|#

(defmethod ndaref ((array ndarray) &rest indices)
  (if (not (type-check-indices indices))
      (error "Invalid index format - ~a" indices))
  (let ((num-indices (num-axes array)))
    (if (or (notevery #'integerp indices)
            (/= (length indices) num-indices))
        (let* ((index-iter indices)
               (slice-indices (loop for i from 0 below num-indices
                                 for shape-idx downfrom (1- num-indices)
                                 collecting (if (null (car index-iter))
                                                (list 0 (vref (shape array) shape-idx))
                                                (if (integerp (car index-iter))
                                                    (list (car index-iter) (1+ (car index-iter)))
                                                    (car index-iter)))
                                 do (setf index-iter (cdr index-iter)))))
          (slice array slice-indices))
        (mref (buffer array) (+ (offset array)
                                (loop for index in indices
                                   for stride-idx downfrom (1- (size (strides array)))
                                   summing (* index (vref (strides array) stride-idx))))))))


