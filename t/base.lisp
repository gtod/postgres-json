(in-package :postgres-json-test)

(defvar *model* 'pgj-test-model)
(defvar *cat* 'pgj-cat)

(defparameter *updates* 12)
(defparameter *workers* 4)
(defparameter *process-results-timeout* 4)

;; Of course key violations are not really OK, but a few are expected
;; given the nature of some testing.
(defparameter *ok-key-violations* 2)

;;;; Impl

(defmacro flatten-errors (() &body body)
  `(handler-case (progn ,@body)
     (error (e)
       (values (format nil "Task error: ~A" e) t))))

(defmacro with-conn (() &body body)
  `(with-connected-thread ()
     (flatten-errors ()
       ,@body)))

(defun maybe-drop (model)
  (when (model-exists-p model)
    (handler-bind ((database-safety-net #'really-do-it))
      (drop-model model))))

(defun call-with-temp-model (model model-parameters thunk)
  (unwind-protect
       (progn
         (create-model model model-parameters)
         (funcall thunk model))
    (maybe-drop model)))

(defmacro with-temp-model ((model &rest mp-args) &body body)
  (with-unique-names (mp)
    `(let ((,mp (make-model-parameters ',model ,@mp-args)))
       (call-with-temp-model ',model ,mp (lambda (,model)
                                           (declare (ignorable ,model))
                                           ,@body)))))

(defun process-results (&optional (timeout *process-results-timeout*))
  (let ((results '()))
    (loop (multiple-value-bind (result has-result-p)
              (flatten-errors ()
                (try-receive-pgj-result :timeout timeout))
            (unless has-result-p
              (return))
            (push result results)))
    (nreverse results)))

(defun insert-some-cats (&optional (number 40))
  (with-conn ()
    (with-model-transaction (some-cats)
      (dotimes (i number (tally *cat*))
        (insert *cat* (obj "name" (format nil "name-~A" i) "coat" "scruffy"))))))

;;;; Interface

(defun setup ()
  (assert *postmodern-connection*)
  (setf *pgj-kernel* (make-pgj-kernel *postmodern-connection* *workers*))
  (ensure-top-level-connection)
  (ensure-backend)
  (ensure-model *model*)
  (ensure-model *cat*)
  (insert-some-cats))

(defun teardown ()
  (with-conn ()
    (handler-bind ((database-safety-net #'really-do-it))
      (maybe-drop *model*)
      (maybe-drop *cat*)))
  (end-pgj-kernel))
