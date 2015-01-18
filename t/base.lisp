(in-package :postgres-json-test)

(define-global-model pgj-test-model -model- (pgj-history-model))
(define-global-model pgj-cat -cat- (pgj-history-model))

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
  (when (backend-exists-p model)
    (handler-bind ((database-safety-net #'really-do-it))
      (drop-backend model))))

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
      (dotimes (i number (tally -cat-))
        (insert -cat- (obj "name" (format nil "name-~A" i) "coat" "scruffy"))))))

(defun call-with-temp-model ())

(defmacro with-temp-model ((instance (&rest superclasses)) &body body)
  (let ((model-name (gensym "PGJ-TEMP-")))
    `(let ((,instance (make-instance (defclass ,model-name (,@superclasses) ()))))
       (unwind-protect
            (progn
              (create-backend ,instance)
              ,@body)
         (maybe-drop ,instance)
         (setf (find-class ',model-name) nil)))))

;;;; Interface

(defun setup ()
  (assert *postmodern-connection*)
  (setf *pgj-kernel* (make-pgj-kernel *postmodern-connection* *workers*))
  (ensure-top-level-connection)
  (ensure-backend -model-)
  (ensure-backend -cat-)
  (insert-some-cats))

(defun teardown ()
  (with-conn ()
    (handler-bind ((database-safety-net #'really-do-it))
      (maybe-drop -model-)
      (maybe-drop -cat-)))
  (end-pgj-kernel))

(defun run-pgj-tests ()
  (setup)
  (unwind-protect (run)
    (teardown)))
