(in-package :aoc)

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

(defun five-part2-p (histogram)
  (let* ((ht (cdr histogram))
         (len (length ht))
         (joker-count (cdr (assoc #\J ht))))
    (cond ((null joker-count) (= 1 len))
          ((= 1 joker-count) (= 2 len))
          ((= 2 joker-count) (= 2 len))
          ((= 3 joker-count) (= 2 len))
          ((= 4 joker-count) (= 2 len))
          ((= 5 joker-count) (= 1 len)))))

(defun four-part2-p (histogram)
  (let* ((ht (cdr histogram))
         (len (length ht))
         (joker-count (cdr (assoc #\J ht))))
    (cond ((null joker-count)
           (dolist (pair (cdr histogram))
             (when (= (cdr pair) 4)
               (return-from four-part2-p t))))
          ((= 1 joker-count)
           (dolist (pair ht)
             (when (= (cdr pair) 3)
               (return t))))
          ((= 2 joker-count)
           (and (= 3 len)
                (dolist (pair ht)
                  (when (= (cdr pair) 2)
                    (return t)))))
          ((= 3 joker-count)
           (= len 3)))))

(defun full-house-part2-p (histogram)
  (let* ((ht (cdr histogram))
         (len (length ht))
         (joker-count (cdr (assoc #\J ht))))
    (if (null joker-count)
        (and (= 2 len)
             (> (cdar ht) 1)
             (< (cdar ht) 4))
        (let ((2-found 0))
          (dolist (pair ht)
            (when (= (cdr pair) 2)
              (setf 2-found (1+ 2-found))))
          ;(format t "~a ~a" joker-count 2-found)
          (and (= 1 joker-count) (= 2 2-found))))))

(defun three-part2-p (histogram)
  (let* ((ht (cdr histogram))
         (len (length ht))
         (joker-count (cdr (assoc #\J ht))))
    (cond ((null joker-count)
           (let ((3-found nil))
             (dolist (pair ht)
               (when (= (cdr pair) 3)
                 (return (setf 3-found t))))
             (and (= 3 len) 3-found)))
          ((= 1 joker-count)
           (let ((2-found nil))
             (dolist (pair ht)
               (when (= (cdr pair) 2)
                 (return (setf 2-found t))))
             (and (= 4 len) 2-found)))
          ((= 2 joker-count)
           (= 4 len)))))

(defun two-part2-p (histogram)
  (let* ((ht (cdr histogram))
         (len (length ht))
         (joker-count (cdr (assoc #\J ht))))
    (when (null joker-count)
      (let ((2-found nil))
        (dolist (pair ht)
          (when (= (cdr pair) 2)
            (return (setf 2-found t))))
        (and (= 3 len) 2-found)))))

(defun one-part2-p (histogram)
  (let* ((ht (cdr histogram))
         (len (length ht))
         (joker-count (cdr (assoc #\J ht))))
    (if (null joker-count)
        (= 4 len)
        (and (= 1 joker-count)
             (= 5 len)))))

(defun card-part2< (card1 card2)
  (cond  ((and (char= #\A card1) (not (char= #\A card2))) nil)
         ((and (char= #\A card2) (not (char= #\A card1))) t)

         ((and (char= #\K card1) (not (char= #\K card2))) nil)
         ((and (char= #\K card2) (not (char= #\K card1))) t)

         ((and (char= #\Q card1) (not (char= #\Q card2))) nil)
         ((and (char= #\Q card2) (not (char= #\Q card1))) t)

         #| Joker is always the smallest |#
         ((char= #\J card1) t)
         ((char= #\J card2) nil)

         ((and (char= #\T card1) (not (char= #\T card2))) nil)
         ((and (char= #\T card2) (not (char= #\T card1))) t)

         (t (< (digit-char-p card1) (digit-char-p card2)))))

(defun hand-part2< (hand1 hand2)
  (let ((h1 (build-histogram hand1))
        (h2 (build-histogram hand2)))
                                        ;(format t "~a~%" h1)
                                        ;(format t "~a~%" h2)
    (cond
      ((string-equal hand1 hand2) t)
      ((and (five-part2-p h1) (not (five-part2-p h2))) nil)
      ((and (five-part2-p h2) (not (five-part2-p h1))) t)

      ((and (four-part2-p h1) (not (four-part2-p h2))) nil)
      ((and (four-part2-p h2) (not (four-part2-p h1))) t)

      ((and (full-house-part2-p h1) (not (full-house-part2-p h2))) nil)
      ((and (full-house-part2-p h2) (not (full-house-part2-p h1))) t)

      ((and (three-part2-p h1) (not (three-part2-p h2))) nil)
      ((and (three-part2-p h2) (not (three-part2-p h1))) t)

      ((and (two-part2-p h1) (not (two-part2-p h2))) nil)
      ((and (two-part2-p h2) (not (two-part2-p h1))) t)

      ((and (one-part2-p h1) (not (one-part2-p h2))) nil)
      ((and (one-part2-p h2) (not (one-part2-p h1))) t)

      (t
       (loop for i from 0 to 4
             do (let ((card1 (char hand1 i))
                      (card2 (char hand2 i)))
                  (when (not (char= card1 card2))
                    (return-from hand-part2< (card-part2< card1 card2)))))))))


(defun reduce2 (function sequence initial-value)
  (let ((result initial-value)
        (index 1))
    (dolist (element sequence result)
      (setq result (funcall function result element index))
      (incf index))))


(defun day7-part2 ()
  (let ((hands (load-hands)))
    (let ((sorted (sort (copy-seq hands)
                        #'hand-part2<
                        :key #'car)))
      (format t "Len1: ~A~%" (length hands))
      (format t "Len2: ~A~%" (length sorted))
      (format t "Diff: ~a" (set-difference hands sorted :test #'equal))
                                        ;(format t "~a~%Reduce: ~%" sorted)
      (with-open-file (stream #P"output7-part2.txt"
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
