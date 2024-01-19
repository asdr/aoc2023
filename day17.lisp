(in-package :cl-user)
(defpackage #:aoc/day17
  (:use :cl)
  (:export #:solve))
(in-package :aoc/day17)


;; Dijkstra's shortest path algirithms 
;; Taken from https://rosettacode.org/wiki/Dijkstra%27s_algorithm#Common_Lisp
;; (defvar *w* '(("a" ("a" "b" . 7) ("a" "c" . 9) ("a" "f" . 14))
;;                     ("b" ("b" "c" . 10) ("b" "d" . 15))
;;                     ("c" ("c" "d" . 11) ("c" "f" . 2))
;;                     ("d" ("d" "e" . 6))
;;                     ("e" ("e" "f" . 9))))
 
(defvar *w* nil)
(defvar *r* nil)
(defvar *max-weight* nil)
(defvar *thread-data* nil)
(defvar *mutex* nil)
                           
(defun dijkstra-short-path (from to)
  (setf *r* nil)
  (paths-1 from to 0 `(,from))
  (sleep 2)
  (consume-threads)
  ;; (format t "td: ~A~%" *thread-data*)
  (let ((found (sort *r* #'< :key #'cadr)))
    (format t "~A" found)
    (car found)))

(defun queue-thread (from to acc-weigth trail)
  (sb-thread:with-mutex (*mutex*)
                          (setf *thread-data* (append *thread-data* (list (list from to acc-weigth trail))))))

(defun consume-threads()
    (loop while *thread-data*
          do (sleep 0.3)
          do (sb-thread:with-mutex (*mutex*)
                                   (sb-thread:make-thread #'paths-1 :arguments (car *thread-data*))
                                   (setf *thread-data* (cdr *thread-data*)))))

(defun paths-1 (from to acc-weigth trail)
    (format t "~a ~a ~a ~a~%" from to acc-weigth trail)
    (if (eql from to) ;;found 
        (push `(,(reverse trail) ,(setf *max-weight* acc-weigth)) *r*)
        (when (< acc-weigth *max-weight*)
          (loop for edge in (nodes from) 
                for edge-to = (cadr edge) do
                (unless (member edge-to trail)
                  (sb-thread:make-thread #'queue-thread :arguments (list edge-to to (+ (cddr edge) acc-weigth) (cons edge-to trail))))))))
 
(defun nodes (vertex-id)
  (sort (cdr (assoc vertex-id *w*)) #'< :key #'cddr))

(defun dijkstra-short-paths (z w) 
  (loop for (a b) in (loop for v on z nconc
                           (loop for e in (cdr v)
                                 collect `(,(car v) ,e)))
        do (setf *r* nil) (paths-2 w a b 0 `(,a))
        (format t "~{Path: ~A  Distance: ~A~}~%"
                (car (sort *r* #'< :key #'cadr)))))
 
(defun paths-2 (w c g z v)
  (if (eql c g) (push `(,(reverse v) ,z) *r*)
      (loop for a in (sort (cdr (assoc c w)) #'< :key #'cddr)
            for b = (cadr a) do (unless (member b v)
                                  (paths-2 w b g (+ (cddr a) z)
                                         (cons b v))))))

(defun test-dijkstra ()
  (format t "Shortest path a-> e: ~A~%" (dijkstra-short-path "a" "e"))
  (dijkstra-short-paths '("a" "b" "c" "d" "e" "f")
                        *w*))
;; end of Dijkstra's shortest path

(defparameter **input-path** #P"input17.txt")
(defparameter **test-input-path** #P"input17-test.txt")
(defparameter **test2-input-path** #P"input17-test2.txt")

(defun vertex-id (x y width)
  (+ x (* y width)))

(defun create-edge (data width from to-x to-y direction)
  (cond ((eql direction :left)
         (when (> to-x -1)
           (cons from (cons (vertex-id to-x to-y width) (aref data to-x to-y)))))
        ((eql direction :right)
         (when (< to-x width)
           (cons from (cons (vertex-id to-x to-y width) (aref data to-x to-y)))))
        ((eql direction :up)
         (when (> to-y -1)
           (cons from (cons (vertex-id to-x to-y width) (aref data to-x to-y)))))
        ((eql direction :down)
         (when (< to-y width)
           (cons from (cons (vertex-id to-x to-y width) (aref data to-x to-y)))))))
        
(defun load-input(&key is-test)
  (let ((data (make-array '(200 200) :element-type 'integer))
        (last-x 0))
    (with-open-file (stream (if is-test (if (eql is-test 2) **test2-input-path** **test-input-path**) **input-path**))
      (loop for line = (read-line stream nil)
            for y from 0
            while line do
            (loop for ch across line
                  for x from 0 do
                  (setf (aref data (setf last-x x) y) (digit-char-p ch)))))
    (loop for y from 0 to last-x do
          (loop for x from 0 to last-x do
                (let ((id (vertex-id x y (1+ last-x))))
                      (let ((left (create-edge data (1+ last-x) id (1- x) y :left))
                            (right (create-edge data (1+ last-x) id (1+ x) y :right)) 
                            (up (create-edge data (1+ last-x) id x (1- y) :up)) 
                            (down (create-edge data (1+ last-x) id x (1+ y) :down)))
                        (let ((vertex (list id)))
                          (when left  (setf vertex (append vertex (list left))))
                          (when right (setf vertex (append vertex (list right))))
                          (when up    (setf vertex (append vertex (list up))))
                          (when down  (setf vertex (append vertex (list down))))
                          (setf *w* (cons vertex *w*)))))))
    (setf *w* (reverse *w*))
    (1+ last-x)))

(defun calculate-starting-weight ()
  100)

(defun find-shortest-path (width &key target)
  (dijkstra-short-path 0 (if target target (1- (* width width)))))

(defun solve (&key is-test target max-weight)
  (declare (optimize (speed 3) (safety 0)))
  ;; (let ((*w* nil)
  ;; (*max-weight* (if max-weight max-weight (calculate-starting-weight))))
  (setf *w* nil)
  (setf *max-weight* (if max-weight max-weight (calculate-starting-weight)))
  (setf *thread-data* nil)
  (setf *mutex* (sb-thread:make-mutex :name "thread-data-lock"))
  
  (let ((width (load-input :is-test is-test)))
    (format t "Width: ~A~%" width)
    (format t "W: ~A~%" *w*)
    (find-shortest-path width :target target)))



