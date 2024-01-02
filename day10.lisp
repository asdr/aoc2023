(in-package :aoc)



(defvar *maze-data* nil)
(defvar *maze-dimensions* nil)
(defvar *nodes* nil)
(defvar *output-stream* t)


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

(defun node-get-location(node)
  (car node))

(defun node-is-starting-node(node)
  (not (null (cadr node))))

(defmacro node-get-char (node)
  `(funcall (third ,node) :operation :get))
(defmacro node-set-char (node value)
  `(funcall (third ,node) :operation :set :value ,value))

(defmacro node-get-west (node)
  `(funcall (fourth ,node) :operation :get))
(defmacro node-set-west (node value)
  `(funcall (fourth ,node) :operation :set :value ,value))

(defmacro node-get-east (node)
  `(funcall (fifth ,node) :operation :get))
(defmacro node-set-east (node value)
  `(funcall (fifth ,node) :operation :set :value ,value))

(defmacro node-get-north (node)
  `(funcall (sixth ,node) :operation :get))
(defmacro node-set-north (node value)
  `(funcall (sixth ,node) :operation :set :value ,value))

(defmacro node-get-south (node)
  `(funcall (seventh ,node) :operation :get))
(defmacro node-set-south (node value)
  `(funcall (seventh ,node) :operation :set :value ,value))


(defun node-to-string(node &optional (prefix "Node: "))
  (if node
      (format *output-stream* "~a ~a ~a \"~a\" ~%"
              prefix
              (node-get-location node)
              (node-is-starting-node node)
              (node-get-char node))
      (format *output-stream* "~A nil~%" prefix)))

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

(defun prepare-node (node)
  (destructuring-bind (row column)
      (node-get-location node)
    (let ((west-char (when (> column 0) (aref *maze-data* row (1- column))))
          (east-char (when (< column (1- (cadr *maze-dimensions*))) (aref *maze-data* row (1+ column))))
          (north-char (when (> row 0) (aref *maze-data* (1- row) column)))
          (south-char (when (< row (1- (car *maze-dimensions*))) (aref *maze-data* (1+ row) column)))
          (west-node (gethash (list row (1- column)) *nodes*))
          (east-node (gethash (list row (1+ column)) *nodes*))
          (north-node (gethash (list (1- row) column) *nodes*))
          (south-node (gethash (list (1+ row) column) *nodes*)))

      (when (node-is-starting-node node)
        nil) ;; TODO: fix the character of starting node

      (if (and west-char
               (null (node-get-west node))
               (position (node-get-char node) "-7J" :test #'char=)
               (position west-char "-FL" :test #'char=))
          (node-set-west node west-node)
          (setf west-node nil))

      (if (and east-char
               (null (node-get-east node))
               (position (node-get-char node) "-FL" :test #'char=)
               (position east-char "-7J" :test #'char=))
          (node-set-east node east-node)
          (setf east-node nil))

      (if (and north-char
               (null (node-get-north node))
               (position (node-get-char node) "|JL" :test #'char=)
               (position north-char "|7F" :test #'char=))
          (node-set-north node north-node)
          (setf north-node nil))

      (if (and south-char
               (null (node-get-south node))
               (position (node-get-char node) "|7F" :test #'char=)
               (position south-char "|JL" :test #'char=))
          (node-set-south node south-node)
          (setf south-node nil))))
  node)

(defun create-graph(&key (starting-pipe #\S))
  (let ((*nodes* (make-hash-table :test #'equal)))
    (create-all-nodes :starting-pipe starting-pipe)
    (loop for row from 0 to (1- (car *maze-dimensions*))
          do (loop for column from 0 to (1- (cadr *maze-dimensions*))
                   do (let ((_node (gethash (list row column) *nodes*)))
                        (prepare-node _node))))
    (let ((starting-location (find-starting-location)))
      (gethash starting-location *nodes*))))

(defun create-all-nodes(&key (starting-pipe #\S))
  (loop for row from 0 to (1- (car *maze-dimensions*))
        do (loop for column from 0 to  (1- (cadr *maze-dimensions*))
                 do(let ((pipe (aref *maze-data* row column)))
                     (create-node (list row column)
                                  :char (if (char= #\S pipe) starting-pipe pipe)
                                  :is-start (char= #\S pipe))))))

(defun traverse-graph (node fn-action &key initial-run skip)
  (when node
    (when (or (not (node-is-starting-node node)) initial-run)
      (let ((west (node-get-west node))
            (east (node-get-east node))
            (north (node-get-north node))
            (south (node-get-south node)))
        (funcall fn-action node)
        (cond ((and (not (eql skip :west)) west)
               (traverse-graph west fn-action :skip :east))
              ((and (not (eql skip :east)) east)
               (traverse-graph east fn-action :skip :west))
              ((and (not (eql skip :north)) north)
               (traverse-graph north fn-action :skip :south))
              ((and (not (eql skip :south)) south)
               (traverse-graph south fn-action :skip :north)))))))

(defun day10-part1 (&key is-test)
  (let* ((*maze-data* (day10-load :load-test is-test))
         (*maze-dimensions* (array-dimensions *maze-data*)))
    (format t "Dimensions: ~a~%" *maze-dimensions*)
    (with-open-file (*output-stream*  #P"output10.txt"
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
      (let ((graph (create-graph :starting-pipe (if is-test #\F #\|)))) ;; TODO: fix starting pipe
        (traverse-graph graph
                        #'(lambda(n)
                            (format *output-stream* "~A: ~A~%" (node-get-location n) (node-get-char n)))
                        :initial-run t)
        graph))))

(defun day10-part2 (&key is-test)
  (let* ((*maze-data* (day10-load :load-test is-test))
         (*maze-dimensions* (array-dimensions *maze-data*)))
    (with-open-file (*output-stream*  #P"output10.txt"
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
      (let ((new-maze (make-array *maze-dimensions* :element-type 'character :initial-element #\.))
            (graph (create-graph :starting-pipe (if is-test #\F #\|))) ;; TODO: fix starting pipe
            (total-number 0))
        (traverse-graph graph
                        #'(lambda(n)
                            (setf (aref new-maze (caar n) (cadar n)) (node-get-char n)))
                        :initial-run t)
        (loop for row from 0 to (1- (car *maze-dimensions*))
              do (let ((row-stack nil))
                   (loop for column from 0 to (1- (cadr *maze-dimensions*))
                         do (let ((ch (aref new-maze row column)))

                              (when (position ch "|JF7L" :test #'char=)
                                (if (null row-stack)
                                    (setf row-stack (cons ch row-stack))
                                    (let ((peek (car row-stack)))
                                      (cond ((and (char= peek #\L) (char= ch #\J))
                                             (setf row-stack (cdr row-stack)))

                                            ((and (char= peek #\F) (char= ch #\7))
                                             (setf row-stack (cdr row-stack)))

                                            ((and (char= peek #\L) (char= ch #\7))
                                             (if (cdr row-stack)
                                                 (setf row-stack nil)
                                                 (setf row-stack (list #\|))))

                                            ((and (char= peek #\F) (char= ch #\J))
                                             (if (cdr row-stack)
                                                 (setf row-stack nil)
                                                 (setf row-stack (list #\|))))

                                            ((char= ch #\|)
                                             (setf row-stack (cdr row-stack)))

                                            (t
                                             (setf row-stack (cons ch row-stack)))))))

                              (let ((new-ch ch))
                                (when (char= ch #\.)
                                  (when row-stack
                                    (setf new-ch #\O)
                                    (incf total-number)))
                                (format *output-stream* "~a" new-ch))

                              (when (= column (1- (cadr *maze-dimensions*)))
                                (format *output-stream* "~%"))))))
        (format *output-stream* "~%~%Total: ~A~%" total-number)))))
