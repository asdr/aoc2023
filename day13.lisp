(defpackage :aoc/day13
  (:nicknames :aoc/day13)
  (:use :cl)
  (:export #:solve))

(in-package :aoc/day13)

(defvar *patterns* nil)

(defun load-input (&key is-test)
  (let ((pattern '(nil)))
    (with-open-file (stream (if is-test #P"input13-test.txt" #P"input13.txt"))
      (loop for line = (read-line stream nil)
            ;; while line
            do (progn 
                 (if (zerop (length line))
                   (progn
                     (format t "~%")
                     (when (car pattern)
                      (setf *patterns* (cons (list (reverse (car pattern))) *patterns*))
                      (setf pattern '(nil))))
                  (progn
                    (format t "~A" line)
                    (setf pattern (list (cons line (car pattern))))))
                 (unless line
                   (return)))))))

(defun build-columns ()
  (let ((patterns nil))
    (loop for pattern in *patterns* do
        (let ((row-count (length (car pattern)))
              (column-count (length (caar pattern)))
              (columns nil))
          (loop for i from 1 to column-count do
                (let ((column ""))
                  (loop for j from 1 to row-count do
                        (setf column (format nil "~a~a" column (char (nth (1- j) (car pattern)) (1- i)))))
                  (setf columns (cons column columns))))
          (setf patterns (cons (list (car pattern) (reverse columns)) patterns))))
    (setf *patterns* patterns)))
                   

(defun solve (&key is-test)
  (let ((*patterns* nil))
    (load-input :is-test is-test)
    (build-columns)
    (format t "~A~%" *patterns*))
  t)
