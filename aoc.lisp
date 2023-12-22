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
  total)





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
  total)


(ql:quickload :cl-ppcre)

(defun check-game-passes (line requirement index)
                                        ;(let (;(line "Game 1: 9 red, 2 green, 13 blue; 10 blue, 2 green, 13 red; 8 blue, 3 red, 6 green; 5 green, 2 red, 1 blue")
                                        ;(red 0)
                                        ;(green 0)
                                        ;(blue 0))
                                        ;(progn
  (let ((game-data (cadr (cl-ppcre:split ":" line))))
    (let ((game-sets (cl-ppcre:split ";" game-data)))
      (if (every #'(lambda (a-set)
                     (let ((color-data (cl-ppcre:split ", " a-set)))
                       (every #'(lambda (a-color-data)
                                  (let ((a-color (string-trim " " a-color-data))
                                        (red1 0)
                                        (green1 0)
                                        (blue1 0))
                                    (cond ((ends-with a-color "red")
                                           (setf red1 (parse-integer (string-trim " red" a-color)))
                                        ;(setf red (+ red red1))
                                           )
                                          ((ends-with a-color "green")
                                           (setf green1 (parse-integer (string-trim " green" a-color)))
                                        ;(setf green (+ green green1))
                                           )
                                          ((ends-with a-color "blue")
                                           (setf blue1 (parse-integer (string-trim " blue" a-color)))
                                        ;(setf blue (+ blue blue1))
                                           ))
                                        ;(format t "G:~A R:~A G:~A B:~A" index red green blue)
                                    (and (<= red1 (car requirement))
                                         (<= green1 (cadr requirement))
                                         (<= blue1 (caddr requirement))))) color-data))) game-sets)
          index
          0))))

;; day2 - part 1
(let ((index 0)
      (total 0))
  (for-each-line #P"input2.txt"
    (setf index (1+ index))
    (setf total (+ total (check-game-passes line '(12 13 14) index))))
  total)


(defun calc-power-of-minimum-set (line)
  (let ((game-data (cadr (cl-ppcre:split ":" line)))
        (red 1)
        (green 1)
        (blue 1))
    (let ((game-sets (cl-ppcre:split ";" game-data)))
      (dolist (a-set game-sets)
        (let ((color-data (cl-ppcre:split ", " a-set)))
          (dolist (a-color-data color-data)
            (let ((a-color (string-trim " " a-color-data)))
              (cond ((ends-with a-color "red")
                     (setf red (max red (parse-integer (string-trim " red" a-color)))))
                    ((ends-with a-color "green")
                     (setf green (max green (parse-integer (string-trim " green" a-color)))))
                    ((ends-with a-color "blue")
                     (setf blue (max blue (parse-integer (string-trim " blue" a-color)))))))))))
    (* red green blue)))

;; day2 - part 2
(let ((index 0)
      (total 0))
  (for-each-line #P"input2.txt"
    (setf index (1+ index))
    (setf total (+ total (calc-power-of-minimum-set line))))
  total)



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
  total)

;; day 3 - part2
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
  total)



(ql:quickload :cl-ppcre)

;; day 4 - part 1
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
  total)





;; day4 - part 2
;; doesnt complete - check O(n)
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
    (format t "~%Result: ~A~%" (+ total-hit-count index))))


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
      (format t "~A~%" total))))

(defun pretty-print-hash-table (hash-table)
  (maphash (lambda (key value)
             (format t "~A: ~A~%" key value))
           hash-table))



;; day 5 - part 1
(defun load-seeds ()
  (with-open-file (stream #P"input5.txt")
    (let* ((line (read-line stream nil))
           (seed-data-str (cadr (cl-ppcre:split "[:] " line))))
      (mapcar #'parse-integer (cl-ppcre:split " " seed-data-str)))))

(defun load-mapping (start end)
  (let ((index  0)
        (soil-data nil))
    (for-each-line #P"input5.txt"
      (setf index (1+ index))
      (when (and (>= index start) (<= index end))
        (setf soil-data (cons (mapcar #'parse-integer (cl-ppcre:split " " line)) soil-data))))
    soil-data))

(defun load-seeds-to-soil ()
  (load-mapping 4 35))

(defun load-soil-to-fertilizer ()
  (load-mapping 38 55))

(defun load-fertilizer-to-water ()
  (load-mapping 58 105))

(defun load-water-to-light ()
  (load-mapping 108 140))

(defun load-light-to-temp ()
  (load-mapping 143 178))

(defun load-temp-to-humidity ()
  (load-mapping 181 209))

(defun load-humidity-to-location ()
  (load-mapping 212 239))


;; this doesnt work due to the size of the given numbers
(defun data-to-map(mapping-data)
  (let ((result-map (make-hash-table)))
    (dolist (mapping mapping-data)
      (let ((dst (car mapping))
            (src (cadr mapping))
            (len (caddr mapping)))
        (format t "Length: ~a~%" len)
                                        ;(loop for i from 0 to (1- len)
                                        ;      do (setf (gethash (+ src i) result-map) (+ dst i)))
        ))
    result-map))

(defun translate-data (from mapping-data)
  (dolist (mapping mapping-data)
    (let ((dst (car mapping))
          (src (cadr mapping))
          (len (caddr mapping)))
      (when (and (>= from src) (< from (+ src len)))
        (return-from translate-data (+ dst (- from src))))))
  from)

(let ((min-location 99999999999999)
      (seeds (load-seeds))
      (mapping-data-to-soil (load-seeds-to-soil))
      (mapping-data-to-fertilizer (load-soil-to-fertilizer))
      (mapping-data-to-water (load-fertilizer-to-water))
      (mapping-data-to-light (load-water-to-light))
      (mapping-data-to-temp (load-light-to-temp))
      (mapping-data-to-humidity (load-temp-to-humidity))
      (mapping-data-to-location (load-humidity-to-location)))
  (dolist (seed seeds)
    (let* ((soil (translate-data seed mapping-data-to-soil))
           (fertilizer (translate-data soil mapping-data-to-fertilizer))
           (water (translate-data fertilizer mapping-data-to-water))
           (light (translate-data water mapping-data-to-light))
           (temp (translate-data light mapping-data-to-temp))
           (humidity (translate-data temp mapping-data-to-humidity))
           (location (translate-data humidity mapping-data-to-location)))
      (format t "~a > ~a > ~a > ~a > ~a > ~a > ~a > ~a ~%" seed soil fertilizer water light temp humidity location)
      (setf min-location (min min-location location))))
  (format t "~a~%" min-location))

;; day5 - part2
(defun load-seeds-2 ()
  (let ((seeds (load-seeds)))
    (loop for (src len) on seeds by #'cddr
          collect (cons src len))))

(defun translate-data-2 (from mapping-data acc)
  (format t "translate-data-2: ~a ~%" from)
                                        ;(dolist (from from-list)
  (dolist (mapping mapping-data)
    (let ((dst (car mapping))
          (src (cadr mapping))
          (len (caddr mapping)))
      (destructuring-bind (from-start . from-len)
          from
        (cond ((<= from-len len)
                                        ;(format t "S: ~a.~a~%" from-start from-len)
               (when (and (>= from-start src) (< from-start (+ src from-len)))
                 (setf acc (cons (cons (+ dst (- from-start src)) from-len) acc)))
               )
              (t
                                        ;(format t "L:~a.~a~%" from-start from-len)
               (translate-data-2 (cons from-start len) mapping-data acc)
               (translate-data-2 (cons (+ from-start from-len) (- from-len len)) mapping-data acc))))))
                                        ;)
  acc)

(let ((min-location 99999999999999)
      (seeds (load-seeds-2))
      (mapping-data-to-soil (load-seeds-to-soil))
      (mapping-data-to-fertilizer (load-soil-to-fertilizer))
      (mapping-data-to-water (load-fertilizer-to-water))
      (mapping-data-to-light (load-water-to-light))
      (mapping-data-to-temp (load-light-to-temp))
      (mapping-data-to-humidity (load-temp-to-humidity))
      (mapping-data-to-location (load-humidity-to-location)))

  (let* ((soils (apply #'append (mapcar #'(lambda(x) (translate-data-2 x mapping-data-to-soil nil)) seeds)))
         )
    ))


                                        ;(dolist (seed seeds)
                                        ;  (let* ((soil (translate-data-2 seeds mapping-data-to-soil nil))
                                        ;         (fertilizer (translate-data-2 soil mapping-data-to-fertilizer nil))
                                        ;         (water (translate-data-2 fertilizer mapping-data-to-water nil))
                                        ;         (light (translate-data-2 water mapping-data-to-light nil))
                                        ;         (temp (translate-data-2 light mapping-data-to-temp nil))
                                        ;         (humidity (translate-data-2 temp mapping-data-to-humidity nil))
                                        ;         (location (translate-data-2 humidity mapping-data-to-location nil)))
                                        ;(format t "~a > ~a > ~a > ~a > ~a > ~a > ~a > ~a ~%" seed soil fertilizer water light temp humidity location)
                                        ;    (dolist (l location)
                                        ;      (setf min-location (min min-location (car l)))))
                                        ;  )
(format t "~a~%" min-location)
