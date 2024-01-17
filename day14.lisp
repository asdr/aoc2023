(defpackage :aoc/day14
  (:nicknames :aoc/day14)
  (:use :cl)
  (:export #:solve))

(in-package :aoc/day14)

(defvar *output-stream* t)
(defvar *platform* nil)

(defun load-input (&key is-test)
  (with-open-file (stream (if is-test #P"input14-test.txt" #P"input14.txt"))
    (let ((rows nil))
      (loop for line = (read-line stream nil)
            while line
            do (setf rows (cons line rows)))
      (setf *platform* (list (reverse rows) (build-columns rows))))))

(defun build-columns (rows)
  (let ((row-count (length rows))
        (column-count (length (car rows)))
        (columns nil))
    ;; (format *output-stream* "Rows: ~a" rows)
    ;; (format *output-stream* " (~a,~a)~%" row-count column-count)     
    ;; (format *output-stream* "~a~%" (car rows))
    (loop for i from 1 to column-count do 
          (let ((column ""))
            (loop for j from 1 to row-count do
                  (setf column (format nil "~a~a" column (char (nth (1- j) rows) (1- i)))))
            (setf columns (cons (reverse column) columns))))
    (reverse columns)))

(defun tilt-platform (&key direction)
  )
    
(defun solve (&key is-test)
  (let ((total 0)
        (*platform* nil))
    (load-input :is-test is-test)
    (tilt-platform :direction :north)
    (format *output-stream* "~A~%" *platform*)
    total))

