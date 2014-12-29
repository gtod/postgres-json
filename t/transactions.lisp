(in-package :postgres-json-transaction-test)

(defvar *model* 'pgj-test-model)
(defvar *cat* 'pgj-cat)

(defparameter *updates* 12)
(defparameter *workers* 4)
(defparameter *process-results-timeout* 4)

;; Of course key violations are not really OK, but a few are expected
;; given the nature of some testing, see below.
(defparameter *ok-key-violations* 2)

;;;; Evaluate (setup), (run), (teardown), all exported symbols of this package

;;;; Impl

(defun really-do-it (c)
  (declare (ignore c))
  (invoke-restart 'really-do-it))

(defmacro flatten-errors (() &body body)
  `(handler-case (progn ,@body)
     (error (e)
       (values (format nil "Task error: ~A" e) t))))

(defmacro with-conn (() &body body)
  `(with-connected-thread ()
     (flatten-errors ()
       ,@body)))

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

;; run is imported from 1am

(defun teardown ()
  (with-conn ()
    (handler-bind ((database-safety-net #'really-do-it))
      (drop-model *model*)
      (drop-model *cat*)))
  (end-pgj-kernel))

;;;; Transactions with isolation levels

(test no-nesting-actual-tran
  (with-transaction-level (foo read-committed-rw)
    (signals cl-postgres:postgresql-warning
      (with-transaction-level (foo read-committed-rw)
        (pomo:query "create temp table foo (bar int)")))))

(test not-nestable-isolation-levels
  (ensure-transaction-level (read-committed-ro)
    (tally *model*)
    (signals postgres-json:incompatible-transaction-setting
      (ensure-transaction-level (repeatable-read-rw)
        (excise *model* 7))))

  (ensure-transaction-level (read-committed-rw)
    (tally *model*)
    (signals postgres-json:incompatible-transaction-setting
      (ensure-transaction-level (serializable-rw)
        (excise *model* 7))))

  (ensure-transaction-level (repeatable-read-rw)
    (tally *model*)
    (ensure-transaction-level (read-committed-ro)
      (keys *model*)
      (signals postgres-json:incompatible-transaction-setting
        (ensure-transaction-level (serializable-rw)
          (excise *model* 7))))))

(test read-only-tran
  (with-transaction-level (foo read-committed-ro)
    (signals cl-postgres:database-error
      (pomo:query "create temp table foo (bar int)"))))

(test manual-tran
  (with-model-transaction (a)
    (excise *model* 7)
    (insert *model* (obj "foo" 17 "bar" #(1 2 3)) :use-key 7)
    (update *model* 7 (obj "foo" 17 "bar" #(1 2 3 4)))
    (is (= 17 (gethash "foo" (fetch *model* 7)))))

  (with-model-transaction (b)
    (excise *model* 7)
    (insert *model* (obj "foo" 17 "bar" #(1 2 3)) :use-key 7)
    (with-model-transaction (c)
      (update *model* 7 (obj "foo" 17 "bar" #(1 2 3 4)))
      (is (= 17 (gethash "foo" (fetch *model* 7)))))))

;; ;;;; Parallel tests

(defun call-with-multiple-updates (thunk)
  (dotimes (i *updates*)
    (funcall thunk)))

(defmacro with-multiple-updates (() &body body)
  `(call-with-multiple-updates (lambda () ,@body)))

(defmacro with-key-update ((key) &body body)
  `(let ((*pgj-channel* (make-pgj-channel))
         (,key (with-conn () (first (keys *cat*)))))
     (flet ((update ()
              (update *cat* ,key (obj "name" (format nil "name-~A" key) "coat" "scruffy"))))
       ,@body)))

(defun count-text-in-list (text list)
  (count-if (lambda (value)
              (and (stringp value)
                   (search text value)))
            list))

(defun key-violation-p (text)
  (and (stringp text)
       (search "Database error 23505" text)))

(defun results-serialization-failure-p (results)
  (not (zerop (count-text-in-list "Database error 40001" results))))

(defun strip-results ()
  (let ((results (process-results)))
    ;; It certainly is not a perfect method...
    (unless (= *updates* (length results))
      (setf results (append results (process-results (* *process-results-timeout* 2)))))
    (let ((stripped (remove-if #'key-violation-p results)))
      (let ((key-violations (- (length results) (length stripped))))
        (unless (zerop key-violations)
          (log:info "Saw ~A key violations.  1 or 2 are possible..." key-violations))
        (values key-violations stripped)))))

;;; Here we update a single record in one of our models *update*
;;; times, all at once by using lparallel.  This is an unusual use
;;; case, but does serve to exercise the serialization failure
;;; handling.  Some key violations in the *cat*_old table are possible
;;; as the primary key there is just the key from the *cat* table plus
;;; the Postgres clock timestamp...

;;; Unfortunately these are non-deterministic because just asking that
;;; serialization failures be handled under the covers doesn't mean
;;; they will be as we do not sleep indefinitely waiting till the
;;; failures disappear.  The idea is that it becomes *very unlikely*
;;; except under *extremely pathological cases*, which means we expect
;;; these tests to pass.

(test default-multiple
  (with-key-update (key)
    (with-multiple-updates ()
      (submit-pgj-task ()
        (update)))
    (multiple-value-bind (key-violations results) (strip-results)
      (is (= *updates* (+ key-violations (length results))))
      (is (>= *ok-key-violations* key-violations))
      (is (= key (reduce #'min results))))))

(test model-tran-multiple
  (with-key-update (key)
    (with-multiple-updates ()
      (submit-pgj-task ()
        (with-model-transaction ()
          (update))))
    (multiple-value-bind (key-violations results) (strip-results)
      (is (= *updates* (+ key-violations (length results))))
      (is (>= *ok-key-violations* key-violations))
      (is (= key (reduce #'min results))))))

(test no-serial-multiple
  (with-key-update (key)
    (with-multiple-updates ()
      (submit-pgj-task ()
        (let ((*serialization-failure-sleep-times* nil))
          (update))))
    (is (results-serialization-failure-p (process-results)))))

(test ensure-tran-multiple
  (with-key-update (key)
    (with-multiple-updates ()
      (submit-pgj-task ()
        ;; We set the right isolation level but fail to handle the
        ;; resulting serialization failures 400001...
        (ensure-transaction-level (repeatable-read-rw)
          (update))))
    (is (results-serialization-failure-p (process-results)))))

;; ;;;; Model transaction handling

(test no-explicit-tran
  (excise *model* 7)
  (insert *model* (obj "foo" 17 "bar" #(1 2 3)) :use-key 7)
  (update *model* 7 (obj "foo" 17 "bar" #(1 2 3 4)))
  (is (= 4 (length (gethash "bar" (fetch *model* 7))))))

(test model-tran
  (with-model-transaction ()
    (excise *model* 7)
    (insert *model* (obj "foo" 17 "bar" #(1 2 3)) :use-key 7)
    (update *model* 7 (obj "foo" 17 "bar" #(1 2 3 4)))
    (is (= 4 (length (gethash "bar" (fetch *model* 7)))))))

(test rollback-1
  (let (key)
    (ignore-errors
     (with-model-transaction (foo)
       (setf key (insert *model* 1234))
       (error "Foo")))
    (is (not (fetch *model* key)))))

(test rollback-2
  (let (key)
    (with-model-transaction (foo)
      (setf key (insert *model* 1234))
      (pomo:abort-transaction foo))
    (is (not (fetch *model* key)))))

(test nested-rollback-1
  (with-model-transaction (a)
    (excise *model* 7))
  (with-model-transaction (b)
    (insert *model* (obj "foo" 17 "bar" #(1 2 3)) :use-key 7)
    (rollback b)
    (is (not (fetch *model* 7))))
  (is (not (fetch *model* 7))))

(test nested-rollback-2
  (with-model-transaction (a)
    (excise *model* 7))
  (with-model-transaction (b)
    (insert *model* 123 :use-key 7)
    (with-model-transaction (c)
      (update *model* 7 124)
      (is (= 124 (fetch *model* 7)))
      (rollback c))
    (is (= 123 (fetch *model* 7))))
  (is (= 123 (fetch *model* 7))))

(test nested-rollback-3
  (with-model-transaction (a)
    (excise *model* 7))
  (with-model-transaction (b)
    (insert *model* 123 :use-key 7)
    (with-model-transaction (c)
      (update *model* 7 124)
      (is (= 124 (fetch *model* 7)))
      (rollback c)
      (is (= 123 (fetch *model* 7))))
    (is (= 123 (fetch *model* 7)))
    (rollback b)
    (is (not (fetch *model* 7))))
  (is (not (fetch *model* 7))))

(test nested-rollback-4
  (with-model-transaction (a)
    (excise *model* 7)
    (with-model-transaction (b)
      (insert *model* (obj "foo" 17 "bar" #(1 2 3)) :use-key 7)
      (with-model-transaction (c)
        (update *model* 7 (obj "foo" 18 "bar" #(1 2 3)))
        (rollback c))
      (is (= 17 (gethash "foo" (fetch *model* 7))))
      (rollback b))
    (is (not (fetch *model* 7)))))

(test nested-commit-1
  (with-model-transaction (a)
    (excise *model* 7))
  (with-model-transaction (b)
    (insert *model* 123 :use-key 7)
    (is (= 123 (fetch *model* 7))))
  (is (= 123 (fetch *model* 7))))

(test nested-commit-2
  (with-model-transaction (a)
    (excise *model* 7))
  (with-model-transaction (b)
    (insert *model* 123 :use-key 7)
    (with-model-transaction (c)
      (update *model* 7 124)
      (is (= 124 (fetch *model* 7))))
    (is (= 124 (fetch *model* 7))))
  (is (= 124 (fetch *model* 7))))

(test nested-commit-3
  (with-model-transaction (a)
    (excise *model* 7))
  (with-model-transaction (b)
    (insert *model* 123 :use-key 7)
    (with-model-transaction (c)
      (update *model* 7 124)
      (is (= 124 (fetch *model* 7)))
      (commit c)
      (is (= 124 (fetch *model* 7))))
    (is (= 124 (fetch *model* 7)))
    (rollback b)
    (is (not (fetch *model* 7))))
  (is (not (fetch *model* 7))))

(test commit-early
  (let (key)
    (with-model-transaction (foo)
      (setf key (insert *model* 1234))
      (commit foo)
      (rollback foo))
    (is (fetch *model* key))))

(test nested-model-tran-1
  (with-model-transaction (a)
    (excise *model* 7)
    (with-model-transaction (b)
      (insert *model* (obj "foo" 17 "bar" #(1 2 3)) :use-key 7)
      (with-model-transaction (c)
        (update *model* 7 (obj "foo" 17 "bar" #(1 2 3)))))
    (is (= 3 (length (gethash "bar" (fetch *model* 7))))))
  (is (= 3 (length (gethash "bar" (fetch *model* 7))))))

(test nested-model-tran-2
  (flet ((update-many (key)
           (with-model-transaction (bar)
             (dotimes (i 10)
               (update *model* key (obj "name" "Joey" "coat" "tabby" "count" i))))))
    (with-model-transaction (foo)
      (excise *model* 6)
      (insert *model* (obj "name" "Joey" "coat" "tabby") :use-key 6)
      (let ((len (length (history *model* 6))))
        (update-many 6)
        (is (= 10 (- (length (history *model* 6)) len)))))))
