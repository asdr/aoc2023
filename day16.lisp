(in-package :cl-user)
(defpackage #:aoc/day16
  (:use :cl)
  (:export #:solve))
(in-package :aoc/day16)

(defvar *grid-data* nil)
(defvar *grid* nil)

(defun load-input (&key is-test)
  (with-open-file (stream (if is-test #P"input16-test.txt" #P"input16.txt"))
    (loop for line = (read-line stream nil)
          while line
          do (setf *grid-data* (cons line *grid-data*))))
  (setf *grid-data* (reverse *grid-data*))
  (let ((row-count (length *grid-data*))
        (column-count (length (car *grid-data*))))
    (setf *grid* (make-array (list row-count column-count)))
    (loop for i from 0 to (1- row-count) do
          (loop for j from 0 to (1- column-count) do
                (setf (aref *grid* i j) (char (nth i *grid-data*) j))))))

(defun traverse-grid()
  (let ((dimensions (array-dimensions *grid*))
        (traversed (make-hash-table :test #'equal)))
    (labels ((traverse (x y direction fn)
               ;; (format t "(~a,~a) ~a [~a,~a] ~%" x y direction (car dimensions) (cadr dimensions))
               (unless (or (= x (car dimensions))
                           (= x -1)
                           (= y (cadr dimensions))
                           (= y -1)
                           (gethash (list x y direction) traversed))
                 (setf (gethash (list x y direction) traversed) t)
                 (let ((ch (aref *grid* y x)))
                   (unless (funcall fn ch x y direction)
                     (return-from traverse))
                   (cond ((char= ch #\.)
                          (cond ((eql direction :left)
                                 (traverse (1- x) y direction fn))
                                ((eql direction :right)
                                 (traverse (1+ x) y direction fn))
                                ((eql direction :up)
                                 (traverse x (1- y) direction fn))
                                ((eql direction :down )
                                 (traverse x (1+ y) direction fn))))
                         ((char= ch #\/)
                          (cond ((eql direction :left)
                                 (traverse x (1+ y) :down fn))
                                ((eql direction :right)
                                 (traverse x (1- y) :up fn))
                                ((eql direction :up)
                                 (traverse (1+ x) y :right fn))
                                ((eql direction :down )
                                 (traverse (1- x) y :left fn))))
                         ((char= ch #\\)
                          (cond ((eql direction :left)
                                 (traverse x (1- y) :up fn))
                                ((eql direction :right)
                                 (traverse x (1+ y) :down fn))
                                ((eql direction :up)
                                 (traverse (1- x) y :left fn))   
                                ((eql direction :down )           
                                 (traverse (1+ x) y :right fn)))) 
                         ((char= ch #\|)
                           (cond ((eql direction :left)
                                  (traverse x (1- y) :up fn)
                                  (traverse x (1+ y) :down fn))
                                 ((eql direction :right)
                                  (traverse x (1- y) :up fn)
                                  (traverse x (1+ y) :down fn))
                                 ((eql direction :up)
                                  (traverse x (1- y) direction fn))
                                 ((eql direction :down )
                                  (traverse x (1+ y) direction fn))))
                         ((char= ch #\-)
                          (cond ((eql direction :left)
                                 (traverse (1- x) y direction fn))
                                ((eql direction :right)
                                 (traverse (1+ x) y direction fn))
                                ((eql direction :up)
                                 (traverse (1- x) y :left fn)
                                 (traverse (1+ x) y :right fn))
                                ((eql direction :down)
                                 (traverse (1- x) y :left fn)
                                 (traverse (1+ x) y :right fn)))))))))
      (let ((count 0)
            (energized (make-hash-table :test #'equal)))
        (traverse 0 0 :right #'(lambda(ch x y direction)
                                 (format t "~a (~a,~a) ~a~%" ch x y direction)
                                 (let ((passed (gethash (list x y) energized)))
                                   (if (not passed)
                                     (progn
                                       (incf count)
                                       (setf (gethash (list x y) energized) 1))
                                     (setf (gethash (list x y) energized) (1+ passed))))
                                 t))
        (format t "Energized: ~a~%" count)))))


(defun solve (&key is-test)
  (let ((*grid-data* nil)
        (*grid* nil))
    (load-input :is-test is-test)
    (format t "~a~%" *grid*)
    (traverse-grid)))


