;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)

;;;; Definition of ndarray (n-dimensional array).

(in-package :cl-numerical.ndarray)

(defclass ndarray ()
  ((num-axes :initarg       :num-axes
             :initform      nil
             :type          'fixnum
             :reader        num-axes
             :documentation "Number of axes")
   (shape    :initarg       :shape
             :initform      (error "Must specify :shape")
             :type          'c-vector
             :reader        shape
             :documentation "Shape of the array")
   (strides  :initarg       :strides
             :initform      (error "Must specify :strides")
             :type          'c-vector
             :reader        strides
             :documentation "Stride across elements for each axis")
   (offset   :initarg       :offset
             :initform      0
             :type          'fixnum
             :reader        offset
             :documentation "Offset to the first element (0, 0, 0, ...)")
   (buffer   :initarg       :buffer
             :initform      (error "Must specify :buffer")
             :type          'buffer
             :reader        buffer
             :documentation "Backing data buffer"))
  (:documentation "N-dimensional array"))

(defmethod size ((array ndarray))
  (shape-to-size (shape array)))

(defmethod inner-stride ((array ndarray))
  (vref (strides array) (1- (num-axes array))))

(defmethod same-type-p ((a ndarray) (b ndarray))
  (equal (ctype (buffer a)) (ctype (buffer b))))

(defun make-ndarray (shape &key (ctype :float) (strides nil) (initial-contents nil) (buffer nil) (offset 0))
  "Create an ndarray object"
  (let* ((shape-vec (cond
                      ((c-vector-p shape) shape)
                      ((listp shape)      (make-c-vector :ctype :int
                                                         :size (length shape)
                                                         :initial-contents (reverse shape)))
                      ((integerp shape)   (make-c-vector :ctype :int
                                                         :size 1
                                                         :initial-contents shape))
                      (t                  (error "Invalid value for shape"))))
         (strides-vec (cond
                        ((null strides)       (shape-to-strides shape-vec))
                        ((c-vector-p strides) strides)
                        ((listp      strides) (make-c-vector :ctype :int
                                                             :size  (length strides)
                                                             :initial-contents strides)))))
    (make-instance 'ndarray
                   :num-axes (size shape-vec)
                   :shape shape-vec
                   :strides strides-vec
                   :buffer (if buffer
                               buffer
                               (let ((size (shape-to-size shape-vec)))
                                 (make-instance 'buffer
                                                :ctype ctype
                                                :size  size
                                                :initial-contents initial-contents)))
                   :offset offset)))

(defun type-check-indices (indices)
  "Ensure that indices is a list of either integers or lists containing two integers"
  (and (loop for index in indices collecting
            (or (integerp index)
                (and (listp index)
                     (= (length index) 2)
                     (integerp (car index))
                     (integerp (cadr index)))))))

(defmethod squeeze ((array ndarray))
  "Squeeze out axis of size 1"
  (let ((num-axes (num-axes array)))
    (let ((shape (make-c-vector-from-list (loop for i from 0 below num-axes
                                             when (/= (vref (shape array) i) 1)
                                             collecting (vref (shape array) i))))
          (strides (make-c-vector-from-list (loop for i from 0 below num-axes
                                               when (/= (vref (shape array) i) 1)
                                               collecting (vref (strides array) i)))))
      (make-ndarray shape
                    :strides strides
                    :offset (offset array)
                    :buffer (buffer array)))))

(defmethod slice ((array ndarray) slice-offsets)
  "Get a slince out of an ndarray"
  (if (not (type-check-indices slice-offsets))
      (error "Invalid index format - ~a" slice-offsets))
  (let* ((shape (mapcar (lambda (x) (abs (- (second x) (first x)))) slice-offsets)))
    (squeeze (make-ndarray shape
                           :strides (strides array)
                           :offset  (+ (offset array) (loop for slice-offset in (reverse slice-offsets)
                                                         for i from 0
                                                         summing (* (car slice-offset) (vref (strides array) i))))
                           :buffer  (buffer array)))))

(defmethod print-1d ((array ndarray) stream)
  "Print out the values in a 1D ndarray"
  (let ((len (size array)))
    (loop for i from 0 below len
       while (< i *MAX-VALS-PRINTED-PER-ROW*)
       initially (format stream "[")
       do (format stream "~a~6f" (if (zerop i) "" ", ") (ndaref array i))
       finally (format stream "~a]" (if (> len *MAX-VALS-PRINTED-PER-ROW*) " ... " "")))))
      
(defmethod print-2d ((array ndarray) stream)
  "Print out the values in a 2D ndarray"
  (let ((rows (vref (shape array) 1)))
    (loop for i from 0 below rows while (< i *MAX-ROWS-PRINTED*)
       do
         (format stream "~a" (if (zerop i) "[" " "))
         (print-1d (ndaref array i) stream)
         (format stream "~a~%" (if (= i (1- rows)) "" ","))
       finally (format stream (if (> rows *MAX-ROWS-PRINTED*) ",~% ...~% ]" "]~%")))))
    
(defmethod print-nd ((array ndarray) stream))

(defmethod print-object ((array ndarray) stream)
  "ndarray printer"
  (format stream "NDARRAY~%")
  (format stream "Shape      : ~a~%" (shape array))
  (format stream "Strides    : ~a~%" (strides array))
  (format stream "Offset     : ~a~%" (offset array))
  (format stream "Size       : ~a~%" (size array))
  (format stream "Buffer size: ~a~%" (size (buffer array)))
  ;;(format stream "Type   : ~a~%" (size array)
  (format stream "Data:~%")
  (cond
    ((= (num-axes array) 1) (print-1d array stream))
    ((= (num-axes array) 2) (print-2d array stream))
    (t                      (print-nd array stream))))
