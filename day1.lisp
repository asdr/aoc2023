(in-package :aoc)

(defmacro for-each-line (file-path &body body)
  `(with-open-file (stream ,file-path)
     (loop for line = (read-line stream nil)
           while line
           do (progn ,@body))))

(defmacro for-each-char (str direction &body body)
  `(cond ((equal ,direction :forward)
          (loop for ch across ,str
                do (progn ,@body)))
         ((equal ,direction :backward)
          (loop for i downfrom (1- (length ,str)) to 0
                do (let ((ch (char ,str i))) ,@body)))))

;; day1 - part1
(defun day1-part1 ()
  (let ((total 0))
    (for-each-line #P"input1.txt"
      (let ((first-digit (for-each-char line :forward
                           (let ((d (digit-char-p ch)))
                             (when d
                               (return d)))))
            (last-digit (for-each-char line :backward
                          (let ((d (digit-char-p ch)))
                            (when d
                              (return d))))))
        (format t "~A~A~%" first-digit last-digit)
        (setf total (+ total last-digit (* 10 first-digit)))))
    total))


;; day1 - part2
(defun starts-with (string prefix)
  "Check if STRING starts with SUFFIX."
  (let ((string-length (length string))
        (prefix-length (length prefix)))
    (and (>= string-length prefix-length)
         (string= (subseq string 0 prefix-length)
                  prefix))))

(defun ends-with (string suffix)
  "Check if STRING ends with SUFFIX."
  (let ((string-length (length string))
        (suffix-length (length suffix)))
    (and (>= string-length suffix-length)
         (string= (subseq string (- string-length suffix-length))
                  suffix))))


(defun digit-in-text-p (text)
  (let ((list '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
    (loop for item in list
          for index from 0
          when (or (ends-with text item) (starts-with text item))
            return index)))

(defun day1-part2 ()
  (let ((total 0))
    (for-each-line #P"input1.txt"
      (let* ((digit-in-text "")
             (first-digit (for-each-char line :forward
                            (let ((d (digit-char-p ch)))
                              (if (not d)
                                  (progn
                                    (setf digit-in-text (format nil "~A~A" digit-in-text ch))
                                    (let ((dit (digit-in-text-p digit-in-text)))
                                      (if dit
                                          (progn
                                            (setf digit-in-text "")
                                            (return dit)))))
                                  (return d)))))
             (last-digit (for-each-char line :backward
                           (let ((d (digit-char-p ch)))
                             (if (not d)
                                 (progn
                                   (setf digit-in-text (format nil "~A~A" ch digit-in-text))
                                   (let ((dit (digit-in-text-p digit-in-text)))
                                     (if dit
                                         (return dit))))
                                 (return d))))))
        (format t "~A" line)
        (format t " +++ ~A~A~%" first-digit last-digit)
        (setf total (+ total last-digit (* 10 first-digit)))))
    total))
