(in-package :aoc)

;; day 4 - part 1
(defun day4-part1 ()
  (let ((index 0)
        (total 0 ))
    (for-each-line #P"input4.txt"
      (setf index (1+ index))
      (let* ((line-data (cl-ppcre:split "[:|]" line))
             (winners-str (string-trim " " (cadr line-data)))
             (own-str (string-trim " " (caddr line-data))))
        (let ((winners (mapcar #'(lambda (x) (parse-integer (string-trim " " x))) (cl-ppcre:split "[ ]+" winners-str)))
              (own (mapcar #'(lambda (x) (parse-integer (string-trim " " x))) (cl-ppcre:split "[ ]+" own-str)))
              (hit-count 0))
                                        ;        (format t "~A: ~A -- ~A~%" index winners own)
          (dolist (w winners)
            (when (find w own)
              (setf hit-count (1+ hit-count))))
          (when (> hit-count 0)
            (setf total (+ total (expt 2 (1- hit-count))))))))
    total))





;; day4 - part 2
;; doesnt complete - check O(n)
(defun day4-part2-attempt1 ()
  (let ((index 0)
        (card-data (make-hash-table)))
    (for-each-line #P"input4.txt"
      (setf index (1+ index))
      (let* ((line-data (cl-ppcre:split "[:|]" line))
             (winners-str (string-trim " " (cadr line-data)))
             (own-str (string-trim " " (caddr line-data)))
             (winners (mapcar #'(lambda (x) (parse-integer (string-trim " " x))) (cl-ppcre:split "[ ]+" winners-str)))
             (own (mapcar #'(lambda (x) (parse-integer (string-trim " " x))) (cl-ppcre:split "[ ]+" own-str))))
        (setf (gethash index card-data) (cons winners own))))
                                        ;(format t "~A~%" card-data)
    (let ((processing-stack (loop for i from 1 to index collect i))
          (total-hit-count 0)
          (saved-hits (make-hash-table)))

      (do ((i 1 (car processing-stack)))
          ((= 0 (length processing-stack)))
        (format t "~A " (length processing-stack))
        (setf processing-stack (cdr processing-stack))

        (multiple-value-bind (found exists)
            (gethash i saved-hits)
          (progn
            (when (and exists (> (car found) 0))
              (setf total-hit-count (+ total-hit-count (car found)))
              (setf processing-stack (append processing-stack (cdr found)))
              (go end-of-loop))))

        (multiple-value-bind (card exists)
            (gethash i card-data)
          (when exists
            (let ((winners (car card))
                  (own (cdr card))
                  (hit-count 0)
                  (hit-data nil))
              (dolist (w winners)
                (when (find w own)
                  (setf hit-count (1+ hit-count))))
              (setf hit-data (cons hit-count nil))
              (when (> hit-count 0)
                (setf total-hit-count (+ total-hit-count hit-count))
                (setf hit-data (cons hit-count (loop for k from (1+ i) to (+ i hit-count) collect k)))
                (setf processing-stack (append processing-stack (cdr hit-data)))
                (setf (gethash i saved-hits) hit-data)))))
       end-of-loop)
      (format t "~%Result: ~A~%" (+ total-hit-count index)))))


;; day4 - part2
;; second atempt

(let ((**saved-hits** (make-hash-table)))
  (defun find-hit-count (i card-data)
    (multiple-value-bind (found exists)
        (gethash i **saved-hits**)
      (cond (exists
             found)
            (t
             (multiple-value-bind (card exists2)
                 (gethash i card-data)
               (when exists2
                 (let ((winners (car card))
                       (own (cdr card))
                       (hit-count 0)
                       (hit-data nil))
                   (dolist (w winners)
                     (when (find w own)
                       (setf hit-count (1+ hit-count))))
                   (setf hit-data (cons hit-count nil))
                   (when (> hit-count 0)
                     (setf hit-data (cons hit-count (loop for k from (1+ i) to (+ i hit-count) collect k)))
                     (setf (gethash i **saved-hits**) hit-data))
                   hit-data))))))))

(defun day4-part2 ()
  (let ((index 0)
        (card-data (make-hash-table)))
    (for-each-line #P"input4.txt"
      (setf index (1+ index))
      (let* ((line-data (cl-ppcre:split "[:|]" line))
             (winners-str (string-trim " " (cadr line-data)))
             (own-str (string-trim " " (caddr line-data)))
             (winners (mapcar #'(lambda (x) (parse-integer (string-trim " " x))) (cl-ppcre:split "[ ]+" winners-str)))
             (own (mapcar #'(lambda (x) (parse-integer (string-trim " " x))) (cl-ppcre:split "[ ]+" own-str))))
        (setf (gethash index card-data) (cons winners own))))
    (let ((lookup-table (make-hash-table)))
      (loop for k from 1 to index
            do (progn
                 (multiple-value-bind (info exists)
                     (gethash k lookup-table)
                   (cond ((not exists)
                          (setf (gethash k lookup-table) (cons 1 nil)))
                         (t
                          (let ((where (cdr info)))
                            (let ((count 1))
                              (dolist (item where)
                                (multiple-value-bind (item-info item-exists)
                                    (gethash item lookup-table)
                                  (when item-exists
                                    (setf count (+ count (car item-info))))))
                              (setf (gethash k lookup-table) (cons count nil)))))))
                 (destructuring-bind (hit-count . bound-items) (find-hit-count k card-data)
                   (when (> hit-count 0)
                     (dolist (item bound-items)
                       (multiple-value-bind (item-info item-exists)
                           (gethash item lookup-table)
                         (if item-exists
                             (setf (gethash item lookup-table) (cons (car item-info) (cons k (cdr item-info))))
                             (setf (gethash item lookup-table) (cons 0 (list k))))))))))
                                        ;(pretty-print-hash-table lookup-table)
      (let ((total 0))
        (maphash #'(lambda (k v) (when k (setf total (+ total (car v))))) lookup-table)
        (format t "~A~%" total)))))

(defun pretty-print-hash-table (hash-table)
  (maphash (lambda (key value)
             (format t "~A: ~A~%" key value))
           hash-table))
