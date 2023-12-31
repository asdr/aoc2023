(in-package :aoc)



(defvar *maze-data* nil)
(defvar *maze-dimensions* nil)
(defvar *nodes* nil)
(defvar *output-stream* t)


(defun day10-load(&key load-test)
  (let ((rows nil)
        (row-count 0))
    (with-open-file (stream (if load-test #P"input10-test.txt" #P"input10.txt" ))
      (loop for line = (read-line stream nil)
            while line
            do (progn
                 (setf rows (cons line rows))
                 (incf row-count))))
    (let ((maze (make-array (list row-count (length (car rows)))
                            :element-type 'character)))
      (loop for row in (reverse rows)
            for i from 0
            do (loop for char across row
                     for j from 0
                     do (setf (aref maze i j) char)))
      maze)))

(defun find-starting-location ()
  (loop for i from 0 to (1- (car *maze-dimensions*))
        do (loop for j from 0 to (1- (cadr *maze-dimensions*))
                 do (when (char= #\S (aref *maze-data* i j))
                      (return-from find-starting-location (list i j))))))

(defun create-node(location &key char west east north south is-start)
  (let ((_location location)
        (_is-start is-start)
        (_char char)
        (_west west)
        (_east east)
        (_north north)
        (_south south))
    (let ((node (list
                 _location
                 _is-start
                 #'(lambda (&key operation value) (if (eql operation :get) _char (setf _char value)))
                 #'(lambda (&key operation value) (if (eql operation :get) _west (setf _west value)))
                 #'(lambda (&key operation value) (if (eql operation :get) _east (setf _east value)))
                 #'(lambda (&key operation value) (if (eql operation :get) _north (setf _north value)))
                 #'(lambda (&key operation value) (if (eql operation :get) _south (setf _south value))))))
      (if *nodes*
          (setf (gethash _location *nodes*) node)
          node))))

(defun node-to-string(node &optional (prefix "Node: "))
  (if node
      (destructuring-bind (location is-start fn-char w e n s)
          node
        (format *output-stream* "~a ~a ~a \"~a\" ~%" prefix location is-start (funcall fn-char :operation :get)))
      (format *output-stream* "~A nil~%" prefix)))


(defun build-graph (starting-node)
  (destructuring-bind ((row column) is-start fn-char fn-west fn-east fn-north fn-south)
      starting-node
    (let ((west-char (when (> column 0) (aref *maze-data* row (1- column))))
          (east-char (when (< column (1- (cadr *maze-dimensions*))) (aref *maze-data* row (1+ column))))
          (north-char (when (> row 0) (aref *maze-data* (1- row) column)))
          (south-char (when (< row (1- (car *maze-dimensions*))) (aref *maze-data* (1+ row) column)))
          (west-node (gethash (list row (1- column)) *nodes*))
          (east-node (gethash (list row (1+ column)) *nodes*))
          (north-node (gethash (list (1- row) column) *nodes*))
          (south-node (gethash (list (1+ row) column) *nodes*)))
      ;; (node-to-string starting-node)

      (if (and west-char
               (null (funcall fn-west :operation :get))
               (position (funcall fn-char :operation :get) "-7J" :test #'char=)
               (position west-char "-FL" :test #'char=))
          (progn
            ;; (node-to-string west-node "Child West: ")
            (funcall fn-west
                     :operation :set
                     :value west-node))
          (setf west-node nil))

      (if (and east-char
               (null (funcall fn-east :operation :get))
               (position (funcall fn-char :operation :get) "-FL" :test #'char=)
               (position east-char "-7J" :test #'char=))
          (progn
            ;; (node-to-string east-node "Child East: ")
            (funcall fn-east
                     :operation :set
                     :value east-node))
          (setf east-node nil))

      (if (and north-char
               (null (funcall fn-north :operation :get))
               (position (funcall fn-char :operation :get) "|JL" :test #'char=)
               (position north-char "|7F" :test #'char=))
          (progn
            ;; (node-to-string north-node "Child North: ")
            (funcall fn-north
                     :operation :set
                     :value north-node))
          (setf north-node nil))

      (if (and south-char
               (null (funcall fn-south :operation :get))
               (position (funcall fn-char :operation :get) "|7F" :test #'char=)
               (position south-char "|JL" :test #'char=))
          (progn
            ;; (node-to-string south-node "Child South: ")
            (funcall fn-south
                     :operation :set
                     :value south-node))
          (setf south-node nil))))
  starting-node)

(defun create-graph(&key (starting-pipe #\S))
  (let ((*nodes* (make-hash-table :test #'equal)))
    (create-all-nodes :starting-pipe starting-pipe)
    (loop for row from 0 to (1- (car *maze-dimensions*))
          do (loop for column from 0 to (1- (cadr *maze-dimensions*))
                   do (let ((_node (gethash (list row column) *nodes*)))
                        (build-graph _node))))
    *nodes*))

(defun create-all-nodes(&key (starting-pipe #\S))
  (loop for row from 0 to (1- (car *maze-dimensions*))
        do (loop for column from 0 to  (1- (cadr *maze-dimensions*))
                 do(let ((pipe (aref *maze-data* row column)))
                     (create-node (list row column)
                                  :char (if (char= #\S pipe) starting-pipe pipe)
                                  :is-start (char= #\S pipe))))))

(defun traverse-graph (node fn &key initial-run skip)
  (when node
    (destructuring-bind (location is-start fn-char fn-west fn-east fn-north fn-south)
        node
      (when (or (null is-start) initial-run)
        (funcall fn node)
        (cond ((and (not (eql skip :west)) (funcall fn-west :operation :get))
               (traverse-graph (funcall fn-west :operation :get) fn :skip :east))
              ((and (not (eql skip :east)) (funcall fn-east :operation :get))
               (traverse-graph (funcall fn-east :operation :get) fn :skip :west))
              ((and (not (eql skip :north)) (funcall fn-north :operation :get))
               (traverse-graph (funcall fn-north :operation :get) fn :skip :south))
              ((and (not (eql skip :south)) (funcall fn-south :operation :get))
               (traverse-graph (funcall fn-south :operation :get) fn :skip :north)))))))


(defun day10-part1 (&key is-test)
  (let* ((*maze-data* (day10-load :load-test is-test))
         (*maze-dimensions* (array-dimensions *maze-data*)))
    (format t "Dimensions: ~a~%" *maze-dimensions*)
    (with-open-file (*output-stream*  #P"output10.txt"
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
      (let ((starting-location (find-starting-location))
            (graph (create-graph :starting-pipe (if is-test #\F #\|))))
        (traverse-graph (gethash starting-location graph)
                        #'(lambda(n)
                            (format *output-stream* "~A: ~A~%" (car n) (funcall (caddr n) :operation :get)))
                        :initial-run t)
        graph))))



(defun day10-part2 (&key is-test)
  (let ((*maze-data* (day10-load :load-test is-test))
        (*maze-dimensions* (array-dimensions *maze-data*)))
    (with-open-file (*output-stream*  #P"output10-part2.txt"
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
      (let ((new-maze (make-array '(140 140) :element-type 'character :initial-element #\.)))
        (traverse-graph (gethash '(60 75) (create-graph))
                        #'(lambda(n)
                            (setf (aref new-maze (caar n) (cadar n)) #\X))
                        :initial-run t)
        (loop for row from 0 to 139
              do (loop for column from 0 to 139
                       do (progn
                            (format *output-stream* "~a" (aref new-maze row column))
                            (when (= column 139)
                              (format *output-stream* "~%")))))))))






(defun day10-part2()
(with-open-file (*output-stream* #P"output10-part2.txt"
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
  (let ((new-maze (make-array '(140 140) )))
    (traverse-graph (gethash '(60 75) (create-graph))
                    #'(lambda(n)
                        (format aoc::*output-stream*
                                "~A: ~A~%"
                                (car n)
                                (funcall (caddr n) :operation :get)))
                    :initial-run t))))
