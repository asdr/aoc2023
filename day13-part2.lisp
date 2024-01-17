(defpackage :aoc/day13/part2
  (:nicknames :aoc/day13/part2)
  (:use :cl)
  (:export #:solve))

(in-package :aoc/day13/part2)

(defvar *patterns* nil)

(defun load-input (&key is-test)
  (let ((pattern '(nil)))
    (with-open-file (stream (if is-test #P"input13-test.txt" #P"input13.txt"))
      (loop for line = (read-line stream nil)
            do (progn 
                 (if (zerop (length line))
                     (when (car pattern)
                      (setf *patterns* (cons (list (reverse (car pattern))) *patterns*))
                      (setf pattern '(nil)))
                    (setf pattern (list (cons line (car pattern)))))
                 (unless line
                   (return))))))
  (build-columns))

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

(defun calculate-diff (s1 s2)
  (let ((diff 0))
    (block outer
      (when (and s1 s2)
        (let ((l1 (length s1)))
          (loop for i from 0 to (1- l1) do
                (unless (char= (char s1 i) (char s2 i))
                  (incf diff)
                  (when (> diff 1)
                    (return-from outer)))))))
        diff))

(defun mirror? (string-list index &key allow-null)
  ;; (format t "mirror? ~A~%" index)
  ;; (format t "~A~%" string-list)
  (when index
    (let ((diffs nil))
      (loop for i downfrom index to 1 
          for k from (+ index 1) to (length string-list) do
          (let ((x (nth (1- i) string-list))
                (y (nth (1- k) string-list)))
            (let ((diff (calculate-diff x y)))
              ;; (format t "~a,~a: ~a =? ~a (diff: ~a)~%" i k x y diff)
              (unless (string= x y)
                (if (> (length diffs) 1)
                    (return-from mirror? nil)
                    (setf diffs (cons diff diffs)))))))
          (and diffs (= (car diffs) 1)))))

(defun find-successive-duplicate (string-list)
  (loop with previous = (first string-list)
        for string in (rest string-list)
        for index from 1
        when (or (and (string= previous string)
                      (mirror? string-list index :allow-null t))
                 (and (= (calculate-diff previous string) 1)
                      (mirror? string-list index :allow-null nil)))
        return (list string index)
        do (setf previous string)))

(defun find-mirror (pattern)
  (let ((h-location (find-successive-duplicate (car pattern))))
    (if h-location
        (progn
          ;; (format t "found: h: ~A~%" h-location)
          (list 'h (cadr h-location)))
        (let ((v-location (find-successive-duplicate (cadr pattern))))
          ;; (format t "found v: ~A~%" v-location)
          (when v-location
            (list 'v (cadr v-location)))))))

(defun calculate-points (row-count column-count mirror-location)
  (if mirror-location
      (if (eql 'v (car mirror-location))
        (cadr mirror-location)
        (* 100 (cadr mirror-location)))
      0))

(defun solve (&key is-test)
  (let ((*patterns* nil)
        (total 0))
    (load-input :is-test is-test)
    (loop for pattern in *patterns* do
          ;; (format t "Pattern: ~a~%" pattern)
          (let ((row-count (length (car pattern)))
                (column-count (length (cadr pattern)))
                (mirror-location (find-mirror pattern)))
            (format t "found-mirror-location: ~a [~a,~a]~%" mirror-location row-count column-count)
            (setf total (+ total (calculate-points row-count column-count mirror-location)))))
    (format t "Total: ~A~%" total)
    total))


