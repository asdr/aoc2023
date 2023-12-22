(in-package :aoc)

(defun pull-number-from-stack (stack step)
  (if stack
      (+ (* (digit-char-p (car stack)) (expt 10 step)) (pull-number-from-stack (cdr stack) (1+ step)))
      0))

(defun express-locations (x y stack-count)
  (when (> stack-count 0)
    (cons (cons (- x stack-count) y) (express-locations x y (1- stack-count)))))


;;day3 - part1
(defun process-line (line index)
  (let ((data nil)
        (char-stack nil)
        (x 0))
    (for-each-char line :forward
      (setf x (1+ x))
      (cond ((char= ch #\.)
             (when char-stack
               (setf data (cons (cons (pull-number-from-stack char-stack 0) (express-locations x index (length char-stack))) data))
               (setf char-stack nil)))
            ((digit-char-p ch)
             (setf char-stack (cons ch char-stack)))
            ;; special chars
            (t
             (when char-stack
               (setf data (cons (cons (pull-number-from-stack char-stack 0) (express-locations x index (length char-stack))) data))
               (setf char-stack nil))
             (setf data (cons (cons ch (list (cons x index))) data)))))
    (when char-stack
      (setf data (cons (cons (pull-number-from-stack char-stack 0) (express-locations (1+ x) index (length char-stack))) data)))
    data))

(defun find-adjecent-numbers (data x y)
  (let ((numbers nil)
        (index (1+ (length data))))
    (dolist (line-data data)
      (setf index (1- index))
      (when (or (= y index) (= index (1+ y)) (= index (1- y)))
                                        ;(format t "asdr")
        (dolist (nb-data line-data)
          (let ((nb (car nb-data))
                (locations (cdr nb-data))
                (collected nil))
                                        ;(format t " ------ ~A : ~A ~%" nb locations)
            (when (and (numberp nb) (find x locations :key #'car :test #'(lambda (b a) (or (= a b) (= a (1- b)) (= a (1+ b))))))
              (when (not (member nb collected))
                (setf numbers (cons nb numbers))))))))
    numbers))

(defun day3-part1 ()
  (let ((index 0)
        (location-data nil)
        (total 0))
    (for-each-line #P"input3.txt"
      (setf index (1+ index))
                                        ;(format t "~A -- " index)
      (let ((ld (process-line line index)))
                                        ;(format t "~A~%" ld)
        (setf location-data (cons ld location-data))))
    (dolist (line-data location-data)
      (dolist (part-data line-data)
        (when (not (numberp (car part-data)))
          (let ((adj-numbers (find-adjecent-numbers location-data (caadr part-data) (cdadr part-data))))
            (setf total (+ total
                           (reduce #'+
                                   adj-numbers
                                   )))
            (format t "~A(~A,~A) :: ~A~%" (car part-data) (caadr part-data) (cdadr part-data) adj-numbers)
            ))))
    total))

;; day 3 - part2
(defun day3-part2 ()
  (let ((index 0)
        (location-data nil)
        (total 0))
    (for-each-line #P"input3.txt"
      (setf index (1+ index))
      (let ((ld (process-line line index)))
        (setf location-data (cons ld location-data))))
    (dolist (line-data location-data)
      (dolist (part-data line-data)
        (when (and (not (numberp (car part-data))) (char= (car part-data) #\*))
          (let ((adj-numbers (find-adjecent-numbers location-data (caadr part-data) (cdadr part-data))))
            (when (= (length adj-numbers) 2)
              (setf total (+ total
                             (reduce #'*
                                     adj-numbers
                                     )))
              )))))
    total))
