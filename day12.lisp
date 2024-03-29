(defpackage :aoc/day12
  (:nicknames :aoc/day12 :aoc.day12)
  (:use :cl)
  (:export
   ))

(in-package :aoc/day12)

(defvar *day12-data* nil)
(defvar **day12-permutations** (make-array 20))

(defun day12-load-input(&key is-test)
  (let ((rows nil))
    (with-open-file (stream (if is-test #P"input12-test.txt" #P"input12.txt"))
      (loop for line = (read-line stream nil)
            while line
            do (setf rows (cons line rows))))
    (setf *day12-data* rows)))

(defun string-empty-p (s)
  (and s (= (length s) 0)))

(defun find-initial-groups()
  (let ((groups nil))
    (loop for line in *day12-data* do
      (let ((line-group (reverse (remove-if #'string-empty-p (cl-ppcre:split "([.]+)|\\s+" line)))))
        (setf line-group (list (mapcar #'parse-integer (cl-ppcre:split "," (car line-group)))
                               (reverse (cdr line-group))
                               (subseq line 0 (position #\Space line))
                               ))
        (setf groups (cons line-group groups))))
    groups))

(defun valid-group-p (definition damage-list)
  (let ((expected-list (mapcar #'(lambda(n) (format nil "~V@{~A~:*~}" n #\#)) damage-list))
	(given-list (remove-if #'string-empty-p (cl-ppcre:split "[.]+" definition))))
    (equal expected-list given-list)))

(defun position-all (item sequence &key (test #'char=) (start 0))
  (let ((positions nil)
	(len (length sequence))
	(s start))
    (do nil
	((>= s len))
      (let ((pos (position item sequence :test test :start s)))
	;;(format t "pos: ~A~%" pos)
	(if pos
	    (progn
	      (setf positions (cons pos positions))
	      (setf s (1+ pos)))
	    (setf s len))))
    (reverse positions)))

(defmacro get-permutations (depth charbag)
  (if (numberp depth)
  `(get-permutations-inner ,depth ,charbag nil)
  `(get-permutations-inner ,(apply (car depth) (cdr depth)) ,charbag nil)))

(defmacro get-permutations-inner (depth charbag vars)
  (if (zerop depth)
      `(setf permutations (cons (format nil ,(format nil "~V@{~A~:*~}" (length vars) "~a") ,@vars) permutations))
      (if vars
	  (let ((var (gensym (format nil "index-~a" depth))))
	    `(loop for ,var across ,charbag do
	      (get-permutations-inner ,(1- depth) ,charbag ,(reverse (cons var (reverse vars))))))
	  `(let ((permutations nil))
	     ,(let ((var (gensym (format nil "index-~a" depth))))
	       `(loop for ,var across ,charbag do
		 (get-permutations-inner ,(1- depth) ,charbag ,(reverse (cons var (reverse vars))))))
	     permutations))))

(setf (aref **day12-permutations** 1) (get-permutations 1 ".#"))
(setf (aref **day12-permutations** 2) (get-permutations 2 ".#"))
(setf (aref **day12-permutations** 3) (get-permutations 3 ".#"))
(setf (aref **day12-permutations** 4) (get-permutations 4 ".#"))
(setf (aref **day12-permutations** 5) (get-permutations 5 ".#"))
(setf (aref **day12-permutations** 6) (get-permutations 6 ".#"))
(setf (aref **day12-permutations** 7) (get-permutations 7 ".#"))
(setf (aref **day12-permutations** 8) (get-permutations 8 ".#"))
(setf (aref **day12-permutations** 9) (get-permutations 9 ".#"))
(setf (aref **day12-permutations** 10) (get-permutations 10 ".#"))
(setf (aref **day12-permutations** 11) (get-permutations 11 ".#"))
(setf (aref **day12-permutations** 12) (get-permutations 12 ".#"))
(setf (aref **day12-permutations** 13) (get-permutations 13 ".#"))
(setf (aref **day12-permutations** 14) (get-permutations 14 ".#"))
(setf (aref **day12-permutations** 15) (get-permutations 15 ".#"))
(setf (aref **day12-permutations** 16) (get-permutations 16 ".#"))
(setf (aref **day12-permutations** 17) (get-permutations 17 ".#"))
(setf (aref **day12-permutations** 18) (get-permutations 18 ".#"))
(setf (aref **day12-permutations** 19) (get-permutations 19 ".#"))

(defun day12-part1(&key is-test)
  (let ((total 0))
    (day12-load-input :is-test is-test)
    (let ((initial-groups (find-initial-groups)))
      (dolist (g initial-groups)
        ;(format t "~a~%" g)
	(let* ((original (third g))
	       (positions (position-all #\? original)))
	  (let ((permutations (aref  **day12-permutations** (length positions))))
	    ;(format t "~a : ~a~%" original positions)
	    ;(format t "Perm: ~a~%" permutations)
	    (loop for perm in permutations do
	      (let ((i -1))
		(loop for ch across perm do
		  (setf (aref original (nth (incf i) positions)) ch)))
	      ;(format t "Before: ~a - ~a : " original (first g))
	      (if (valid-group-p original (first g))
		  (progn
		    (incf total)
		    ;(format t " passed~%")
		    )
		  ;(format t " failed~%")
		  ))))))
    total))

(defun is-valid (expected data)
  (labels ((find-max-group-count (d)
	     (let ((len (length d)))
	       (loop for i from -1 to (- len 2)
		     for j from 0 to (- len 1) do
		       (let ((prev (when (> i -1) (char d i)))
			     (cur (char d j)))
			 (when (char= cur #\?)
			   (if (or (null prev) (char= prev #\.))
			       (setf (aref d j) #\#)
			       (setf (aref d j) #\.)))))
	       (1+ (length (position-all #\. (cl-ppcre:regex-replace-all "[.]+" (string-trim "." d) ".")))))))
    (let ((input-data (copy-seq data)))
      (>= (find-max-group-count input-data) expected))))

(defun traverse-node (fn &key props accessor (hist (make-hash-table :test 'equal)))
  (unless (gethash (car props) hist)
    (setf (gethash (car props) hist) t)
    (when (and props (third props))
      (funcall fn props)
      (loop for i from 0 to (1- (cadr props)) do
	(destructuring-bind (c-props c-accessor c-traverser)
	    (funcall accessor i)
	  (declare (ignore c-traverser))
	  (traverse-node fn :props c-props :accessor c-accessor :hist hist))))))

(defun ncreate-node (data expected &optional (level 0) (table (make-hash-table :test 'equal)))
  (let ((_data (copy-seq data))
	(_children (make-array 300))
	(_children-count 0))
    (let ((_valid (is-valid expected _data)))
      (when (and _data _valid)
	(let ((node (gethash _data table)))
	  (if node
	      node
	      (progn
		;(format t "create: ~A[~A][is-valid:~a]~%" data level _valid)
		(let ((positions (position-all #\? _data)))
		  (loop for pos in positions do
		    (setf data (replace data "#" :start1 pos))
		    (let ((child (ncreate-node (copy-seq data) expected (1+ level) table)))
		      (when child
			(setf (aref _children _children-count) child)
			(incf _children-count)))
		    (setf data (replace data "." :start1 pos))
		    (let ((child (ncreate-node (copy-seq data) expected (1+ level) table)))
		      (when child
			(setf (aref _children _children-count) child)
			(incf _children-count)))
		    (setf data (replace data "?" :start1 pos))))
		(let ((props (list _data _children-count _valid))
		      (accessor #'(lambda (index)
				    (when (< index _children-count)
				      (aref _children index)))))
		  (setf (gethash _data table)
			(list props
			      accessor
			      #'(lambda (fn)
				  (traverse-node fn :props props :accessor accessor))))))))))))

(defun day12-part2(&key is-test)
  (let ((total 0))
    (day12-load-input :is-test is-test)
    (let ((initial-groups (find-initial-groups)))
      (dolist (g initial-groups)
        (format t "~a~%" g)
	(funcall
	 (third
	  (aoc/day12::ncreate-node (third g) (length (car g))))
	 
	 #'(lambda(props)
	     (when (and (third props)
			(zerop (cadr props)))
	       (let ((d (car props)))
		 (when (equal (car g)
			      (mapcar #'length (cl-ppcre:split "[.]+" (string-trim "." d))))
		   (format t "~a~%" d)
		   (incf total))))))
	))
    total))

