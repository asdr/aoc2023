(in-package :aoc)

(defvar *day11-data-row-count* 0)
(defvar *day11-data-column-count* 0)
(defvar *day11-data* nil)

(defun day11-load-input(&key is-test)
  (let ((rows nil))
    (with-open-file (stream (if is-test #P"input11-test.txt" #P"input11.txt"))
      (loop for line = (read-line stream nil)
            while line
            do (setf rows (cons line rows))))

    (setf *day11-data-row-count* (length rows))
    ;; (format t "~A," *day11-data-row-count*)
    (setf *day11-data-column-count* (length (car rows)))
    ;; (format t "~A~%" *day11-data-column-count*)
    (setf *day11-data* (make-array (list *day11-data-row-count* *day11-data-column-count*) :element-type 'character))

    (loop for row in rows
          for x from (1- *day11-data-row-count*) downto 0 do
            (loop for ch across row
                  for y from 0 do
                    (progn
                      ;; (format t "~a,~a~%" x y)
                      (setf (aref *day11-data* x y) ch))))))

(defun find-empty-rows()
  (let ((rows nil))
    (loop for row from 0 to (1- *day11-data-row-count*) do
      (unless
          (loop for column from 0 to (1- *day11-data-column-count*) do
            (when (not (char= #\. (aref *day11-data* row column)))
              (return t)))
        (setf rows (cons row rows))))
    rows))

(defun find-empty-columns()
  (let ((columns nil))
    (loop for column from 0 to (1- *day11-data-column-count*) do
      (unless
          (loop for row from 0 to (1- *day11-data-row-count*) do
            (when (not (char= #\. (aref *day11-data* row column)))
              (return t)))
        (setf columns (cons column columns))))
    columns))

(defun find-all-galaxies()
  (let ((galaxies nil)
        (index 0))
    (loop for row from 0 to (1- *day11-data-row-count*) do
      (loop for column from 0 to (1- *day11-data-column-count*) do
        (when (char= #\# (aref *day11-data* row column))
          (setf galaxies (cons (list (incf index) row column) galaxies)))))
    galaxies))

(defun expand-universe(galaxies &optional (coefficient 2))
  (let ((er (find-empty-rows))
        (ec (find-empty-columns))
        (expanded nil))
    (dolist (g galaxies)
      (setf expanded (cons (list
                            (car g)
                            (+ (cadr g) (* (1- coefficient) (length (remove-if #'(lambda(c) (> c (cadr g))) er))))
                            (+ (caddr g) (* (1- coefficient) (length (remove-if #'(lambda(r) (> r (caddr g))) ec)))))
                           expanded)))
    expanded))

(defun get-combinations(galaxies)
  (let ((len (length galaxies))
        (pairs nil))
    (loop for i from 1 to (- len 1) do
      (loop for j from (1+ i) to len do
        (setf pairs (cons (list
                           (find i galaxies :key #'car)
                           (find j galaxies :key #'car))
                          pairs))))
    pairs))

(defun day11-part1(&key is-test)
  (let ((*day11-data-row-count* 0)
        (*day11-data-column-count* 0)
        (*day11-data* nil))
    (day11-load-input :is-test is-test)
    ;; (format t "~a~%" *day11-data-row-count*)
    ;; (format t "~a~%" *day11-data-column-count*)
    (let ((galaxies (find-all-galaxies)))
      ;;(format t "~a~%" galaxies)
      (reduce #'+
              (mapcar #'(lambda(pair)
                          (destructuring-bind ((n1 x1 y1) (n2 x2 y2))
                              pair
                            (+ (abs (- x2 x1))
                               (abs (- y2 y1)))))
                      (get-combinations (expand-universe galaxies)))))))

(defun day11-part2(&key is-test (coefficient 2))
  (let ((*day11-data-row-count* 0)
        (*day11-data-column-count* 0)
        (*day11-data* nil))
    (day11-load-input :is-test is-test)
    ;; (format t "~a~%" *day11-data-row-count*)
    ;; (format t "~a~%" *day11-data-column-count*)
    (let ((galaxies (find-all-galaxies)))
      ;;(format t "~a~%" galaxies)
      (reduce #'+
              (mapcar #'(lambda(pair)
                          (destructuring-bind ((n1 x1 y1) (n2 x2 y2))
                              pair
                            (+ (abs (- x2 x1))
                               (abs (- y2 y1)))))
                      (get-combinations (expand-universe galaxies coefficient)))))))
