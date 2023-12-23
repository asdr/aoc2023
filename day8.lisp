(in-package :aoc)

(defvar *day8-input-file-path* #P"input8.txt")

(defun day8-load-instructions ()
  (with-open-file (stream *day8-input-file-path*)
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
