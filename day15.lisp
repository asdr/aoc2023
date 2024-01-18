
(defpackage :aoc/day15
  (:nicknames :aoc/day15)
  (:use :cl)
  (:export #:solve #:solve-part2))

(in-package :aoc/day15)

(defvar *steps* nil)
(defvar *step-count* 0)
(defvar *sum* 0)

(defun load-steps (&key is-test)
  (with-open-file (stream (if is-test #P"input15-test.txt" #P"input15.txt"))
   (let ((step ""))
      (loop for ch = (read-char stream nil)
            ;; for i from 1
            while ch
            do (if (char= ch #\,)
                   (let ((hash-value (calculate-hash step)))
                     ;; (format t "XXXX: ~a = ~a ~%" step hash-value)
                     (setf (gethash step *steps*) hash-value)
                     (setf *step-count* (1+ *step-count*))
                     (setf *sum* (+ *sum* hash-value))
                     (setf step ""))
                   (unless (char= ch #\Newline)
                    (setf step (format nil "~a~a" step ch)))))
      ;; (format t "aaaaa ~a" step)
      (let ((hash-value (calculate-hash step)))
        ;; (format t "OOOO: ~a = ~a ~%" step hash-value)
        (setf (gethash step *steps*) hash-value)
        (setf *step-count* (1+ *step-count*))
        (setf *sum* (+ *sum* hash-value))))))

(defun calculate-hash (step)
  (let ((hash-value 0))
    (loop for ch across step do
          (setf hash-value (mod (* 17 (+ hash-value (char-code ch))) 256)))
    ;; (format t "XX~aXX --> YY~aYY~%" step hash-value)
    hash-value))

(defun solve (&key is-test)
  (let ((*steps* (make-hash-table :test #'equal))
        (*step-count* 0)
        (*sum* 0))
    (load-steps :is-test is-test)
    *sum*))

(defun solve-part2 (&key is-test)
  0)

