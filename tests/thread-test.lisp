;;;; Some informal lparallel tests for the REPL

;;;; For the :timeout option to work you need a version of
;;;; bordeaux-threads *after* 0.8.3, which is the version in quicklisp
;;;; as at Dec 2014.  I suggest you clone
;;;; https://github.com/sionescu/bordeaux-threads to your QL
;;;; local-projects if the QL bordeaux-threads is still at 0.8.3

;;;; Set your own *connection* values, compile this file and follow
;;;; the instructions under Interface below.

(defpackage :thread-test
  (:use :cl :postgres-json))

(in-package :thread-test)

(defparameter *connection* '("cusoon" "gtod" "" "localhost" :port 5433))

(log:config :info)

;;;; Implementation

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

(defun update-cat (model key)
  (submit-pgj-task ()
    (update model key (obj "name" (format nil "name-~A" key) "coat" "scruffy"))))

;; This is just an example of how easy lparallel makes it to deal with
;; errors in a worker at the point of receiving the results because
;; we ask for them to be transferred --- see SUBMIT-PGJ-FUNCTION.
(defun process-results (&optional (timeout 0.5))
  (log:info "Processing results")
  (loop (multiple-value-bind (result has-result-p)
            (flatten-errors ()
              (try-receive-pgj-result :timeout timeout))
          (unless has-result-p
            (return))
          (log:info "Result: ~A" result))))

;; Not sure how sensible this is but it's food for thought...
(defun process-n-results (n)
  (log:info "Processing results")
  (dotimes (i n)
    (let ((result (flatten-errors ()
                    (receive-pgj-result))))
      (log:info "Result: ~A" result))))

;;;; Interface

;; Run first
(defun start-test (&optional (workers 5))
  (setf *pgj-kernel* (make-pgj-kernel *connection* workers))
  (setf *pgj-channel* (make-pgj-channel))
  (with-conn ()
    (ensure-backend)
    (ensure-model 'pj-cat)))

;; Run second
(defun insert-some-cats (&optional (number 40))
  (with-conn ()
    (with-model-transaction (some-cats)
      (dotimes (i number (tally 'pj-cat))
        (insert 'pj-cat (obj "name" (format nil "name-~A" i) "coat" "scruffy"))))))

;; Run third
(defun update-all ()
  (let ((keys (with-conn () (keys 'pj-cat))))
    (dolist (key keys)
      (log:info "Updating ~A" key)
      (update-cat 'pj-cat key))
    (process-results)))

;; Run fourth
;; In production code you would certainly not expect to see 20
;; different users all trying to update a single record at once.  But
;; this is an interesting, completely informal, stress test of the
;; serialization handling retry code.  Of course, there is no
;; guarantee that all the results are processed, even with a timeout
;; of say 8 seconds...
(defun update-one (&optional (n 20))
  (log:config :debug)
  (let ((key (with-conn () (first (keys 'pj-cat)))))
    (dotimes (i n)
      (log:info "Updating ~A" key)
      (update-cat 'pj-cat key))
    (process-results 8)
    (log:config :info)
    (with-conn () (history 'pj-cat key))))

;; Run fifth
(defun update-broken-1 ()
  (with-conn ()
    (let ((key (first (keys 'pj-cat))))
      (update 'bar key (obj "foo" "bar")))))

;; Run sixth
(defun update-broken-2 ()
  (let ((keys (with-conn () (keys 'pj-cat))))
    (let ((key (first keys)))
      (update-cat 'foo key)
      (update-cat 'pj-cat key)
      (update-cat 'baz key)
      (process-results))))

;; Run seventh
(defun update-broken-3 ()
  (let ((keys (with-conn () (keys 'pj-cat))))
    (let ((key (first keys)))
      (update-cat 'foo key)
      (update-cat 'pj-cat key)
      (update-cat 'baz key)
      (process-n-results 3))))

;; Run last
(defun end-test ()
  (with-conn ()
    (handler-bind ((database-safety-net #'really-do-it))
      (drop-model 'pj-cat)))
  (end-pgj-kernel)
  (log:config :info))
