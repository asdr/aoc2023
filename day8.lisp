(in-package :aoc)

(defun day8-load-instructions (&optional is-for-test)
  (with-open-file (stream (if is-for-test #P"input8_test.txt" #P"input8.txt"))
    (let ((instructions (read-line stream nil)))
      (read-line stream nil)
      (let ((path-data nil))
        (loop for line = (read-line stream nil)
              while line
              do (let ((step (cl-ppcre:split "\\s[=]\\s" line)))
                   (setf path-data (cons
                                    (cons
                                     (car step)
                                     (mapcar #'(lambda(d) (string-trim "( )" d))
                                             (cl-ppcre:split "[,]\\s" (cadr step))))
                                    path-data))))
        (values instructions path-data)))))


(defun count-steps-until (target)
  (with-open-file (stream #P"output8.txt"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (multiple-value-bind (instructions path-data)
        (day8-load-instructions)
      (let ((step-count 0)
            (current-step (assoc "AAA" path-data :test #'string-equal))
            (instruction-list (coerce instructions 'list)))
        (tagbody
         traverse-starts
           (dolist (instruction instruction-list)
             (format stream "~A: ~A (~A)~%" instruction current-step step-count)
             (if  (not (string-equal target (car current-step)))
                  (progn
                    (incf step-count)
                    (if (char= #\L instruction)
                        (setf current-step (assoc (cadr current-step) path-data :test #'string-equal))
                        (setf current-step (assoc (caddr current-step) path-data :test #'string-equal))))
                  (return)))
           (when (and
                  (not (string-equal target (car current-step)))
                  (< step-count 150000000))
             (go traverse-starts)))
        (format stream "Step count: ~A~%" step-count)
        step-count))))

(defun day8-part1()
  (count-steps-until "ZZZ"))


(defun find-starting-nodes(path-data)
  (let ((starting-nodes nil))
    (dolist (node path-data starting-nodes)
                                        ;(format t "~A~%" node)
      (when (char= #\A (car (last (coerce (car node) 'list))))
        (setf starting-nodes (cons node starting-nodes))))))



(defun ends-with-Z (instruction nodes)
  (every #'(lambda(n)
             (if (char= #\L instruction)
                 (char= #\Z (car (last (coerce (cadr n) 'list))))
                 (char= #\Z (car (last (coerce (caddr n) 'list))))))
         nodes))

(defun next-step (instruction nodes path-data)
  (let ((next nil))
    (dolist (n nodes)
      (if (char= #\L instruction)
          (setf next (cons (assoc (cadr n) path-data :test #'string-equal) next))
          (setf next (cons (assoc (caddr n) path-data :test #'string-equal) next))))
    (reverse next)))


#| Traverse sequentially -- taking ages |#
(defun day8-part2 ()
  (with-open-file (stream #P"output8.txt"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (multiple-value-bind (instructions path-data)
        (day8-load-instructions)
      (let ((step-count 0)
            (current-steps (find-starting-nodes path-data))
            (instruction-list (coerce instructions 'list)))
        (tagbody
         traverse-starts
           (when (and
                  (null (dolist (instruction instruction-list)
                          #|
                          (format stream "~A: ~85A (~A)~%" instruction current-steps step-count) ;
                          (force-output stream)
                          |#
                          (if (not (ends-with-Z instruction current-steps))
                              (progn
                                (incf step-count)
                                (setf current-steps (next-step instruction current-steps path-data)))
                              (return t))))
                  (< step-count 15000))
             (go traverse-starts)))

        (format stream "Last step: ~A: ~A~%" current-steps)
        (format stream "Step count: ~A~%" step-count)
        step-count))))

#| Using threads and a hashtable to determine stop scenario |#
#| However, it still takes ages to complete |#
(defun day8-part2-threads()
  (multiple-value-bind (instructions path-data)
      (day8-load-instructions)
    (let* ((found nil)
           (found-map (make-hash-table :synchronized t))
           (current-nodes (find-starting-nodes path-data))
           (number-of-paths (length current-nodes))
           (instruction-list (coerce instructions 'list)))

      (format t "Starting nodes: ~A(~A)~%" current-nodes number-of-paths)
      (format t "Instructions: ~A~%" instruction-list)

      (labels ((start-for-one (node)
                 (let ((step-count 0)
                       (current-step node)
                       (inst-list (copy-seq instruction-list)))
                   (format t "~A: ~A~%" (sb-thread:thread-name sb-thread:*current-thread*) current-step)
                   (labels ((next (instruction)
                              (if (char= #\L instruction)
                                  (setf current-step (assoc (cadr current-step) path-data :test #'string-equal))
                                  (setf current-step (assoc (caddr current-step) path-data :test #'string-equal)))))

                     (sleep 1)
                     #| This is an infinite loop to be executed on a separate thread |#
                     (tagbody
                      traverse-starts
                        (dolist (instruction inst-list)
                                        ;(sleep 10)
                                        ;(format t "~A for ~A~%" instruction current-step)
                          (when (not found)
                            (incf step-count)

                            #| DO things here |#
                            (when (ends-with-Z instruction (list current-step))
                                        ;(format t "Ends with Z~%")
                              (let ((found-ones (gethash step-count found-map)))
                                (setf found-ones (cons current-step found-ones))
                                (setf (gethash step-count found-map) found-ones)
                                        ;(format t "~A:~A~%" (length found-ones) number-of-paths)
                                (when (= (length found-ones) number-of-paths)
                                  (setf found step-count))))

                            #| Go to next step |#
                            (next instruction)))
                        (when (not found)
                          (go traverse-starts)))))))
        (let ((threads nil)
              (index 0))
          (dolist (node current-nodes)
                                        ;(run-thread index node))
            (setf threads (cons
                           (sb-thread:make-thread #'start-for-one
                                                  :name (format nil "Thread~a" (incf index))
                                                  :arguments (list node))
                           threads)))
          (dolist (thread threads)
            (sb-thread:join-thread thread)))
        found))))

(defun convert-alist-to-hashtable(alist)
  (let ((hash-table (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash-table) pair))
    hash-table))

#| Not really working fine .... |#
(defun day8-part2-optimizations ()
  (multiple-value-bind (instructions path-data)
      (day8-load-instructions)
    (let ((found nil)
          (found-nodes (make-array 1000000))
          (number-of-found-nodes 0)
          (found-nodes-lock (sb-thread:make-mutex :name "LOCK"))
          (current-nodes (find-starting-nodes path-data))
          (instruction-list (coerce instructions 'list))
          (path-data-hashtable (convert-alist-to-hashtable path-data)))

      (format t "Starting nodes: ~A~%" current-nodes)
      (format t "Instructions: ~A~%" instruction-list)

      (labels ((add-found-node (new-value)
                 (sb-thread:with-mutex (found-nodes-lock)
                   (setf (aref found-nodes number-of-found-nodes) new-value)
                   (incf number-of-found-nodes)))

               (producer (node)
                 (let ((step-count 0)
                       (current-step node)
                       (inst-list (copy-seq instruction-list)))
                   (format t "~A: ~A~%" (sb-thread:thread-name sb-thread:*current-thread*) current-step)
                   (labels ((next (instruction)
                              (if (char= #\L instruction)
                                  (setf current-step (gethash (cadr current-step) path-data-hashtable))
                                  (setf current-step (gethash (caddr current-step) path-data-hashtable)))))

                     (sleep 2)

                     (tagbody
                      traverse-starts
                        (dolist (instruction inst-list)
                          (when (not found)
                            (incf step-count)

                            (when (ends-with-Z instruction (list current-step))
                              (add-found-node step-count))

                            (next instruction)))
                        (when (not found)
                          (go traverse-starts))))))

               (consumer ()
                 (let ((counts (make-hash-table))
                       (len (length current-nodes)))
                   (do nil              ;nothing to iterate
                       ((not (null found)))
                     (when (> number-of-found-nodes 0)
                       (sb-thread:with-mutex (found-nodes-lock)
                         (let ((s1 (aref found-nodes (decf number-of-found-nodes))))
                           (if s1
                               (let ((c1 (gethash s1 counts)))
                                 (if c1
                                     (setf (gethash s1 counts) (1+ c1))
                                     (setf (gethash s1 counts) 1))
                                 (when (and (not (null c1)) (= c1 len))
                                   (setf found s1)))
                               (setf (gethash s1 counts) 1)))
                         ))
                    loopend))))
        (let ((threads nil)
              (index 0))
          (dolist (node current-nodes)
            (setf threads (cons
                           (sb-thread:make-thread #'producer
                                                  :name (format nil "Producer~a" (incf index))
                                                  :arguments (list node))
                           threads)))
          (sb-thread:join-thread (sb-thread:make-thread #'consumer
                                                        :name (format nil "Consumer~a" (incf index))))))
      found)))

(defvar **last-step** (make-array 6))

#| Another optimization attempt... |#
(defun day8-part2-speed ()
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (instructions path-data)
      (day8-load-instructions)
    (let* ((current-nodes (find-starting-nodes path-data))
           (path-count (length current-nodes))
           (instruction-list (coerce instructions 'list))
           (path-data-hashtable (convert-alist-to-hashtable path-data))
           (found nil)
           (array-of-found-nodes (make-array path-count))
           (array-of-found-node-lengths (make-array path-count))
           (array-of-locks (make-array path-count)))

      (format t "Starting nodes: ~A~%" current-nodes)
      (format t "Instructions: ~A~%" instruction-list)

      #| initialize arrays |#
      (loop for i from 0 to (1- path-count) do
        (setf (aref array-of-locks i) (sb-thread:make-mutex
                                       :name (format nil "LOCK~a" i)))
        (setf (aref array-of-found-node-lengths i) 0)
        (setf (aref array-of-found-nodes i) (make-hash-table)))

      (labels((node-ends-with-Z (instruction node)
                (let ((target (if (char= #\L instruction)
                                  (cadr node)
                                  (caddr node))))
                  (char= #\Z (char target (1- (length target))))))
              (other-threads-found (index step-count)
                (loop for i from 0 to (1- path-count) do
                  (unless (= i index)
                    (unless (gethash step-count (aref array-of-found-nodes i))
                      (return-from other-threads-found nil)
                      )))
                t)
              (traverse-path (index node)
                (let ((step-count 0)
                      (current-step node)
                      (inst-list (copy-seq instruction-list))
                      (found-map (aref array-of-found-nodes index)))

                  (sleep (* index 1))

                  (format t "~A: ~A~%" (sb-thread:thread-name sb-thread:*current-thread*) current-step)
                  (labels ((next (instruction)
                             (if (char= #\L instruction)
                                 (setf current-step (gethash (cadr current-step) path-data-hashtable))
                                 (setf current-step (gethash (caddr current-step) path-data-hashtable)))))

                    #| This is an infinite loop to be executed on a separate thread |#
                    (tagbody
                     traverse-starts
                       (dolist (instruction inst-list)

                         (when (not found)
                           (incf step-count)

                           (setf (aref **last-step** index) step-count)

                           #| DO things here |#
                           (when (node-ends-with-Z instruction current-step)
                             (setf (gethash step-count found-map) step-count)
                             (when (other-threads-found index step-count)
                               (setf found step-count))))

                         #| Go to next step |#
                         (next instruction))
                       (when (not found)
                         (go traverse-starts)))))))
        (let ((threads nil)
              (index 0))
          (dolist (node current-nodes)
                                        ;(run-thread index node))
            (setf threads (cons
                           (sb-thread:make-thread #'traverse-path
                                                  :name (format nil "Thread~a" (incf index))
                                                  :arguments (list (1- index) node))
                           threads)))
          (dolist (thread threads)
            (sb-thread:join-thread thread))))
      found)))

(defun print-last-steps ()
  (do nil (nil)
    (loop for i from 0 to 5 do
      (format t "~A | " (aref **last-step** i)))
    (format t "~%")
    (sleep 10)))

(defun day8-part2-lcm ()
  (multiple-value-bind (instructions path-data)
      (day8-load-instructions)
    (let ((current-steps (find-starting-nodes path-data))
          (instruction-list (coerce instructions 'list))
          (found-steps nil))

      (dolist (path current-steps)
        (let ((step-count 1))

          (setf found-steps (cons (block tb
                                    (tagbody
                                     traverse-starts
                                       (when (and
                                              (null (dolist (instruction instruction-list nil)
                                        ;(format t "~A(~A): ~A~%" instruction step-count path)
                                                      (if (not (ends-with-Z instruction (list path)))
                                                          (progn
                                                            (incf step-count)
                                                            (setf path (car (next-step instruction (list path) path-data))))
                                                          (return t))))
                                              t)
                                         (go traverse-starts))
                                       (return-from tb step-count)))
                                  found-steps))))

      (format t "Found: ~A~%" found-steps)
      (format t "Result: ~A~%" (apply #'lcm found-steps)))))
