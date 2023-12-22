(in-package :aoc)

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

(defun day5-part1()
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
    (format t "~a~%" min-location)))

;; day5 - part2
(defun load-seeds-2 ()
  (let ((seeds (load-seeds)))
    (loop for (src len) on seeds by #'cddr
          collect (cons src len))))

;; takes  65 minutes to complete
;; 57451709
(defun day5-part2()
  (declare (optimize (speed 3) (safety 0)))
  (let ((min-location 99999999999999)
        (seeds (load-seeds-2))
        (mapping-data-to-soil (load-seeds-to-soil))
        (mapping-data-to-fertilizer (load-soil-to-fertilizer))
        (mapping-data-to-water (load-fertilizer-to-water))
        (mapping-data-to-light (load-water-to-light))
        (mapping-data-to-temp (load-light-to-temp))
        (mapping-data-to-humidity (load-temp-to-humidity))
        (mapping-data-to-location (load-humidity-to-location)))
    (dolist (seed seeds)
      (destructuring-bind (seed-start . len)
          seed
                                        ;(format t "~a ~a~%" seed-start len)
                                        ;(go end-loop)
        (loop for s from seed-start to (1- (+ seed-start len)) by 1
              do
                 (let* ((soil (translate-data s mapping-data-to-soil))
                        (fertilizer (translate-data soil mapping-data-to-fertilizer))
                        (water (translate-data fertilizer mapping-data-to-water))
                        (light (translate-data water mapping-data-to-light))
                        (temp (translate-data light mapping-data-to-temp))
                        (humidity (translate-data temp mapping-data-to-humidity))
                        (location (translate-data humidity mapping-data-to-location)))
                                        ;(format t "~a > ~a > ~a > ~a > ~a > ~a > ~a > ~a ~%" seed soil fertilizer water light temp humidity location)
                   (setf min-location (min min-location location)))))
      end-loop)
    (format t "~a~%" min-location)))

(defun translate-data-2 (input mapping-data acc)
  (format t "td: ~a ++ ~a~%" input acc)
  (dolist (mapping mapping-data)
    (let ((dst (car mapping))
          (src (cadr mapping))
          (len (caddr mapping)))
      (destructuring-bind (input-start . input-len)
          input
        (let ((src-diff (- src input-start)))
          (when (>= input-len src-diff)
            (stef acc (cons
                       (cons
                        (if (>= src-diff 0) src input-start)
                        (- input-len src-diff))
                       acc))))))))


                                        ;(if (<= input-len len)
                                        ;   (when (and (>= input-start src) (< input-start (+ src input-len)))
                                        ;    (setf acc (cons (cons (+ dst (- input-start src)) input-len) acc)))
                                        ; (progn
                                        ;  (translate-data-2 (cons input-start len) mapping-data acc)
                                        ; (translate-data-2 (cons (+ input-start input-len) (- input-len len)) mapping-data acc))))))
                                        ;acc)


                                        ;(defun day5-part2 ()
                                        ;  (let ((min-location 99999999999999)
                                        ;        (seeds (load-seeds-2))
                                        ;        (mapping-data-to-soil (load-seeds-to-soil))
                                        ;        (mapping-data-to-fertilizer (load-soil-to-fertilizer))
                                        ;        (mapping-data-to-water (load-fertilizer-to-water))
                                        ;        (mapping-data-to-light (load-water-to-light))
                                        ;        (mapping-data-to-temp (load-light-to-temp))
                                        ;        (mapping-data-to-humidity (load-temp-to-humidity))
                                        ;        (mapping-data-to-location (load-humidity-to-location)))
                                        ;    (let* ((soils (apply #'append (mapcar #'(lambda(x) (translate-data-2 x mapping-data-to-soil nil)) seeds)))
                                        ;           )
                                        ;      )))


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
                                        ;(format t "~a~%" min-location)

(defun run-until (function timeout-seconds)
  (let ((thread (sb-thread:make-thread function))
        (start-time (get-universal-time)))
    (loop
      (when (> (- (get-universal-time) start-time) timeout-seconds)
        (sb-thread:terminate-thread thread)
                                        ;(error "Function execution timed out.")
        )
      (when (not (sb-thread:thread-alive-p thread))
        (return))
      (sleep 1))))
