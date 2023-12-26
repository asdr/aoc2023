(in-package :aoc)

(defun day9-part1-load-data(&key is-for-test is-part-two)
  (let ((data nil))
    (with-open-file (stream (if is-for-test #P"input9_test.txt" #P"input9.txt"))
      (loop for line =(read-line stream nil)
            while line
            do (setf data (cons (if is-part-two
                                    (mapcar #'parse-integer
                                            (cl-ppcre:split "\\s+" line))
                                    (reverse (mapcar
                                              #'parse-integer
                                              (cl-ppcre:split "\\s+" line))))
                                data))))
    (reverse data)))


(defun prepare-diff (history &optional (diff nil))
  (if (cadr history)
      (prepare-diff (cdr history) (cons (- (car history) (cadr history)) diff))
      (reverse diff)))

(defun predict (history)
  (labels ((predict-inner(h upper-h)
             (if (every #'zerop h)
                 (car upper-h)
                 (if (null (cadr h))
                     (return-from predict nil)
                     (if upper-h
                         (+ (car upper-h) (predict-inner (prepare-diff h) h))
                         (predict-inner (prepare-diff h) h))))))
    (predict-inner history nil)))


(defun day9-part1 ()
  (let ((dataset (day9-part1-load-data :is-for-test nil :is-part-two t))
        (total 0))
    (dolist (history dataset)
      (let ((prediction (predict history)))
        (when prediction
          (incf total prediction))))
    total))
