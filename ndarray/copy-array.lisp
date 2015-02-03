;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-NUMERICAL.NDARRAY -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)

;;;; Functions for copying ndarrays.

(in-package :cl-numerical.ndarray)

(defun copy-to (destination source)
  (assert (shape= destination source))
  (let ((shape-dest (shape destination))
        (strides-dest (strides destination))
        (buffer-dest (buffer destination))
        (offset-dest (offset destination))
        
        (shape-src (shape source))
        (strides-src (strides source))
        (buffer-src (buffer source))
        (offset-src (offset source)))
    (let* ((num-axes (num-axes source))
           (loc-dest (make-array num-axes :element-type 'fixnum :initial-element 0))
           (loc-src (make-array num-axes :element-type 'fixnum :initial-element 0))
           (loop-complete nil))
      (macrolet
          ((increment (offset loc shape strides)
             `(progn
                (format t "~a~%" ,loc)
                (incf (aref ,loc 0))
                (setf ,offset (+ ,offset (vref ,strides 0)))

                (let ((i 0))
                  (loop while (and (< i num-axes) (= (aref ,loc i) (vref ,shape i))) do
                       (setf (aref ,loc i) 0)
                       (setf ,offset (- ,offset (* (vref ,shape i) (vref ,strides i))))
                       (incf i)
                       (when (< i num-axes)
                         (incf (aref ,loc i))
                         (setf ,offset (+ ,offset (vref ,strides 0)))))
                  (when (= i num-axes)
                    (setf loop-complete t))))))
                       
                                   
                       
        (loop while (not loop-complete) do
             (setf (mref buffer-dest offset-dest) (mref buffer-src offset-src))
             (increment offset-dest loc-dest shape-dest strides-dest)
             (increment offset-src loc-src shape-src strides-src))))))

        
  
  
