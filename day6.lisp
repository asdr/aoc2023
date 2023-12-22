(in-package :aoc)

(defun load-races ()
  (with-open-file (stream #P"input6.txt")
    (let ((times-str (read-line stream nil))
          (distances-str (read-line stream nil)))
      (mapcar #'(lambda(t1 d1) (cons (parse-integer t1) (parse-integer d1)))
              (cl-ppcre:split "\\s+" (string-trim "TimeDistance: " times-str))
              (cl-ppcre:split "\\s+" (string-trim "TimeDistance: " distances-str))
              )
      )))

(defun calc-number-of-ways-to-win (time distance)
  (let ((wait (floor (/ time 2))))
    (format t "Wait = ~A " wait)
    (let ((first-fail 0))
      (loop
        for k from wait downto 0
        do (when (>= distance (* (- time k) k))
             (setf first-fail k)
             (return)))
      (format t "First-fail = ~A " first-fail)
      (if (evenp time)
          (progn
            (format t "Result = ~A~%" (1- (* 2 (- wait first-fail))))
            (1- (* 2 (- wait first-fail))))
          (progn
            (format t "Result = ~A~%" (* 2 (- wait first-fail)))
            (* 2 (- wait first-fail)))))))

(defun day6-part1 ()
  (reduce
   #'*
   (mapcar
    #'(lambda(race)
        (destructuring-bind (time . distance)
            race
          (calc-number-of-ways-to-win time distance)))
    (load-races))))

(defun load-races-p2 ()
  (with-open-file (stream #P"input6.txt")
    (let ((times-str (read-line stream nil))
          (distances-str (read-line stream nil)))
      (mapcar #'(lambda(t1 d1) (cons (parse-integer t1) (parse-integer d1)))
              (list (cl-ppcre:regex-replace-all "\\s+" (string-trim "TimeDistance: " times-str) ""))
              (list (cl-ppcre:regex-replace-all "\\s+" (string-trim "TimeDistance: " distances-str) ""))
              )
      )))

(defun day6-part2 ()
  (reduce
   #'*
   (mapcar
    #'(lambda(race)
        (destructuring-bind (time . distance)
            race
          (calc-number-of-ways-to-win time distance)))
    (load-races-p2))))
