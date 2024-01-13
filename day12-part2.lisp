#| The algorithm is taken from https://github.com/womogenes/AoC-2023-Solutions/blob/main/day_12/day_12_p2.py |#

(defpackage :aoc/day12/part2
  (:nicknames :aoc/day12/part2 :aoc.day12p2)
  (:use :cl)
  (:export #:solve))

(in-package :aoc/day12/part2)

(defvar *input-data* nil)

(defun join (delimiter &rest strings)
  (format nil (concatenate 'string "~{~A~^" delimiter "~}") strings))

(defun load-input (&key is-test)
  (let ((data nil))
    (with-open-file (stream (if is-test #P"input12-test.txt" #P"input12.txt"))
      (loop for line = (read-line stream nil)
            while line
	    unless (position #\X line :test #'char=)
            do (let ((pair (cl-ppcre:split "\\s+" line)))
		 (setf data
		       (cons (cons
			      (apply #'join
				     (append '("?") (make-list 5 :initial-element (car pair))))
			      (apply #'append
				    (make-list 5 :initial-element (mapcar #'parse-integer (cl-ppcre:split "[,]" (cadr pair))))))
			     data)))))
    (setf *input-data* (reverse data))))

(defun position-all (item sequence &key (test #'char=) (start 0) end)
  (let ((positions nil)
	(len (length sequence))
	(s start))
    (do nil
	((>= s len))
      (let ((pos (position item sequence :test test :start s :end end)))
	;;(format t "pos: ~A~%" pos)
	(if pos
	    (progn
	      (setf positions (cons pos positions))
	      (setf s (1+ pos)))
	    (setf s len))))
    (reverse positions)))

(defun find-last-non-zero (array i j-length)
  (loop for j downfrom (1- j-length) to 0
	do (let ((item (aref array i j 0)))
	     (when item
	       (return-from find-last-non-zero item))))
  0)

(defun find-possible-ways (pair)
  (let ((original (format nil "~a." (car pair)))
	(groups (append (cdr pair) '(0))))
    (let ((original-length (length original))
	  (groups-length (length groups))
	  (max-group-length (apply #'max groups)))
      (let ((d-matrix (make-array
		       (list original-length groups-length (1+ max-group-length))
		       :element-type t
		       )))
	(loop for i from 0 to (1- original-length) do
	  (let ((ch (char original i)))
	    (loop for j from 0 to (1- groups-length) do
	      (loop for k from 0 to (nth j groups) do
		(block innermost-loop
		  (when (zerop i)
		    (unless (zerop j)
		      (setf (aref d-matrix i j k) 0)
		      (return-from innermost-loop))
		    (when (char= ch #\#)
		      (unless (= k 1)
			(setf (aref d-matrix i j k) 0)
			(return-from innermost-loop))
		      (setf (aref d-matrix i j k) 1)
		      (return-from innermost-loop))
		    (when (char= ch #\.)
		      (unless (zerop k)
			(setf (aref d-matrix i j k) 0)
			(return-from innermost-loop))
		      (setf (aref d-matrix i j k) 1)
		      (return-from innermost-loop))
		    (when (char= ch #\?)
		      (unless (position k '(0 1))
			(setf (aref d-matrix i j k) 0)
			(return-from innermost-loop))
		      (setf (aref d-matrix i j k) 1)
		      (return-from innermost-loop)))
		  (let ((for-dot 0)
			(for-hash 0))
		    (cond ((not (zerop k)) (setf for-dot 0))
			  ((> j 0)
			   (setf for-dot (aref d-matrix (1- i) (1- j) (nth (1- j) groups)))
			   (setf for-dot (+ for-dot (aref d-matrix (1- i) j 0))))
			  (t (setf for-dot (if (position-all #\# original :end i) 0 1))))
		    (if (zerop k)
			(setf for-hash 0)
			(setf for-hash (aref d-matrix (1- i) j (1- k))))

		    (cond ((char= ch #\.) (setf (aref d-matrix i j k) for-dot))
			  ((char= ch #\#) (setf (aref d-matrix i j k) for-hash))
			  (t (setf (aref d-matrix i j k) (+ for-dot for-hash)))))
		  )))))
	(find-last-non-zero d-matrix (1- original-length) groups-length)))))

(defun solve(&key is-test)
  (let ((total 0)
	(*input-data* nil)
	(*input-data-groups* nil))
    (dolist (pair (load-input :is-test is-test) total)
      (let ((val (find-possible-ways pair)))
	(incf total val)))
    (format t "Total: ~A~%" total)
    total))
