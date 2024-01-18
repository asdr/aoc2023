(defpackage :aoc/day14
  (:nicknames :aoc/day14)
  (:use :cl)
  (:export #:solve #:solve-part2))

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

(defun tilt-one (string &key direction)
  ;; (format *output-stream* "~%tilt-one: ~A~%" string)
  (let ((groups (cl-ppcre:split "[#]" (format nil "~aX" string)))
        (left-predicate #'(lambda (x y)
                            (declare (ignore y))
                            (char= x #\O)))
        (right-predicate #'(lambda (x y)
                             (declare (ignore y))
                             (char= x #\.))))
    (let ((sorted nil))
      (loop for g in groups do
        (let ((sg (sort (remove #\X g) (if (eql direction :left) left-predicate right-predicate))))
          ;; (format *output-stream* "~a -> ~a~%" g sg)
          (setf sorted (cons sg sorted))))
      (subseq (reduce #'(lambda(x y) (format nil "~a#~a" x y)) (reverse sorted) :initial-value "") 1))))
  

(defun tilt-platform (&key direction)
  (let ((rows (car *platform*))
        (columns (cadr *platform*)))
    (cond ((eql direction :north)
           (setf columns (mapcar #'(lambda (c) (tilt-one c :direction :left)) columns))
           (setf rows (build-columns (reverse columns))))
          ((eql direction :south)
           (setf columns (mapcar #'(lambda (c) (tilt-one c :direction :right)) columns))
           (setf rows (build-columns (reverse columns))))
          ((eql direction :west)
           (setf rows (mapcar #'(lambda (c) (tilt-one c :direction :left)) rows))
           (setf columns (build-columns (reverse rows))))
          ((eql direction :east)
           (setf rows (mapcar #'(lambda (c) (tilt-one c :direction :right)) rows))
           (setf columns (build-columns (reverse rows)))))
    (setf *platform* (list rows columns))))

(defun calculate-total-load()
  (let ((total-load 0)
        (rows (car *platform*))
        (columns (cadr *platform*)))
    (let ((row-count (length rows)))
      (loop for row in rows 
            for index downfrom row-count do
            (incf total-load (* index (count #\O row :test #'char=)))))
    total-load))

(defun solve (&key is-test)
  (let ((*platform* nil))
    (load-input :is-test is-test)
    (tilt-platform :direction :north)
    ;; (format *output-stream* "~A~%" *platform*)
    (calculate-total-load)))

(defvar *cache* nil)

(defun spin-cycle-platform()
  (let ((before *platform*))
    (let ((saved (gethash before *cache*)))
      (if saved
          (setf *platform* saved)
          (progn
            ;; (setf before (copy-seq *platform*))
            (tilt-platform :direction :north)
            (tilt-platform :direction :west)
            (tilt-platform :direction :south)
            (tilt-platform :direction :east)
            (setf (gethash before *cache*) *platform*))))))

 (defun solve-part2 (&key is-test (cycle-count 1))
   (let ((*platform* nil)
         (*cache* (make-hash-table :test #'equal)))
     (load-input :is-test is-test)
     (loop for i from 1 to cycle-count do
           (spin-cycle-platform))
     (calculate-total-load)))

 
