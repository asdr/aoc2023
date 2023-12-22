(in-package :aoc)

(defun load-hands ()
  (with-open-file (stream #P"input7.txt")
    (let ((hands nil))
      (loop for line = (read-line stream nil)
            while line
            do (setf hands (cons (cl-ppcre:split "\\s" line) hands)))
      hands)))

(defun build-histogram (hand)
  (let ((ht nil)
        (previous-pair nil))
    (loop for i from 0 to 4
          do (let* ((sorted-hand (sort (copy-seq hand) #'char<))
                    (card (char sorted-hand i))
                    (pair (assoc card ht)))
                                        ;(format t "~a ~a ~a~%" card pair previous-pair)
               (if (and pair (eq previous-pair pair))
                   (rplacd previous-pair (1+ (cdr previous-pair)))
                   (progn
                     (setf previous-pair (cons card 1))
                     (setf ht (cons previous-pair ht))))))
    (cons hand ht)))

(defun five-p (histogram)
  (= 1 (length (cdr histogram))))

(defun four-p (histogram)
  (dolist (pair (cdr histogram))
    (when (= (cdr pair) 4)
      (return-from four-p t)))
  nil)

(defun full-house-p (histogram)
  (and (= 2 (length (cdr histogram)))
       (> (cdar (cdr histogram)) 1)
       (< (cdar (cdr histogram)) 4)))


(defun three-p (histogram)
  (let ((len (length (cdr histogram)))
        (3-found nil))
    (dolist (pair (cdr histogram))
      (when (= (cdr pair) 3)
        (return (setf 3-found t))))
    (and (= 3 len)
         3-found)))

(defun two-p (histogram)
  (let ((len (length (cdr histogram)))
        (2-found nil))
    (dolist (pair (cdr histogram))
      (when (= (cdr pair) 2)
        (return (setf 2-found t))))
    (and (= 3 len)
         2-found)))

(defun one-p (histogram)
  (= 4 (length (cdr histogram))))

(defun card< (card1 card2)
  (cond  ((and (char= #\A card1) (not (char= #\A card2))) nil)
         ((and (char= #\A card2) (not (char= #\A card1))) t)

         ((and (char= #\K card1) (not (char= #\K card2))) nil)
         ((and (char= #\K card2) (not (char= #\K card1))) t)

         ((and (char= #\Q card1) (not (char= #\Q card2))) nil)
         ((and (char= #\Q card2) (not (char= #\Q card1))) t)

         ((and (char= #\J card1) (not (char= #\J card2))) nil)
         ((and (char= #\J card2) (not (char= #\J card1))) t)

         ((and (char= #\T card1) (not (char= #\T card2))) nil)
         ((and (char= #\T card2) (not (char= #\T card1))) t)

         (t (< (digit-char-p card1) (digit-char-p card2)))))




(defun hand< (hand1 hand2)
  (let ((h1 (build-histogram hand1))
        (h2 (build-histogram hand2)))
                                        ;(format t "~a~%" h1)
                                        ;(format t "~a~%" h2)
    (cond
      ((string-equal hand1 hand2) t)
      ((and (five-p h1) (not (five-p h2))) nil)
      ((and (five-p h2) (not (five-p h1))) t)

      ((and (four-p h1) (not (four-p h2))) nil)
      ((and (four-p h2) (not (four-p h1))) t)

      ((and (full-house-p h1) (not (full-house-p h2))) nil)
      ((and (full-house-p h2) (not (full-house-p h1))) t)

      ((and (three-p h1) (not (three-p h2))) nil)
      ((and (three-p h2) (not (three-p h1))) t)

      ((and (two-p h1) (not (two-p h2))) nil)
      ((and (two-p h2) (not (two-p h1))) t)

      ((and (one-p h1) (not (one-p h2))) nil)
      ((and (one-p h2) (not (one-p h1))) t)

      (t
       (loop for i from 0 to 4
             do (let ((card1 (char hand1 i))
                      (card2 (char hand2 i)))
                  (when (not (char= card1 card2))
                    (return-from hand< (card< card1 card2)))))))))


(defun reduce2 (function sequence initial-value)
  (let ((result initial-value)
        (index 1))
    (dolist (element sequence result)
      (setq result (funcall function result element index))
      (incf index))))


(defun day7-part1 ()
  (let ((hands (load-hands)))
    (let ((sorted (sort (copy-seq hands)
                        #'hand<
                        :key #'car)))
      (format t "Len1: ~A~%" (length hands))
      (format t "Len2: ~A~%" (length sorted))
      (format t "Diff: ~a" (set-difference hands sorted :test #'equal))
                                        ;(format t "~a~%Reduce: ~%" sorted)
      (with-open-file (stream #P"output7.txt"
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (let ((total-winnings (reduce2
                               #'(lambda (acc hand rank)
                                   (format stream "~9a - ~11a - ~4a~%" acc hand rank)
                                   (+ acc (* rank (parse-integer (cadr hand)))))
                               sorted
                               0)))
          (format stream "Total winnings: ~a~%" total-winnings)
          total-winnings)))))
