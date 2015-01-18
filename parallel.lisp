(in-package :postgres-json)

(defvar *pgj-database* nil
  "Thread local Postmodern database connection.")

(defun set-default-search-path ()
  (let ((pomo:*database* *pgj-database*))
    (pomo:set-search-path *default-search-path*)))

(defun ensure-connected ()
  (unless (pomo:connected-p *pgj-database*)
    (log:trace "Reconnecting...")
    (pomo:reconnect *pgj-database*)
    (set-default-search-path)))

(defun worker-context (worker-loop)
  (set-default-search-path)
  (funcall worker-loop)
  (pomo:disconnect *pgj-database*))

;;; The only mutable global state we keep in the running image is the
;;; cache of *query-functions*.  So these need to become thread local.

;;; The neatest solution is to ensure that worker thread : postmodern
;;; connection is 1 : 1, so that's what we do.  Consider pgpool II for
;;; more sophisticated needs.

(defun worker-bindings (connect-spec)
  `((*standard-output* . ,*standard-output*)
    (*error-output*    . ,*error-output*)
    (*pgj-database* . (apply #'pomo:connect ',connect-spec))
    (*query-functions* . (make-hash-table :test #'equal))))

(defun make-pgj-task (function)
  (lambda ()
    (ensure-connected)
    (let ((pomo:*database* *pgj-database*))
      (funcall function))))

;;;; Interface

(defvar *pgj-kernel* nil
  "An lparallel kernel to manage worker threads.  Typically bound to
the result of MAKE-PGJ-KERNEL for use by interface calls such
WITH-CONNECTED-THREAD.")

(defvar *pgj-channel* nil
  "A single lparallel channel for submitting tasks via SUBMIT-PGJ-TASK
and receiving results via RECEIVE-PGJ-RESULT.")

(defun make-pgj-kernel (connect-spec &optional (n 4))
  "Make an lparallel kernel object where each worker thread is given a
permanent DB connection, made using a Postmodern CONNECT-SPEC, a list.
Start N workers.  Ensure your Postgres can handle at least N
concurrent connecions."
  (lparallel:make-kernel n :bindings (worker-bindings connect-spec)
                           :context #'worker-context))

(defun end-pgj-kernel ()
  "End the lparallel kernel in *PGJ-KERNEL*."
  (let ((lparallel:*kernel* *pgj-kernel*))
    (lparallel:end-kernel)))

(defun make-pgj-channel ()
  "Make an lparallel channel.  *PGJ-KERNEL* must be bound to the
result of MAKE-PGJ-KERNEL."
  (let ((lparallel:*kernel* *pgj-kernel*))
    (lparallel:make-channel)))

;;; Blocking function and wrapper macro

(defun call-with-connected-thread (function)
  "Ask that an lparallel worker perform FUNCTION, a function, given a
current Postmodern DB connection.  Block until the result is received
and return it.  *PGJ-KERNEL* must be bound to the result of
MAKE-PGJ-KERNEL."
  (let* ((lparallel:*kernel* *pgj-kernel*)
         (channel (lparallel:make-channel)))
    (lparallel:task-handler-bind ((error 'lparallel:invoke-transfer-error))
      (lparallel:submit-task channel (make-pgj-task function)))
    (lparallel:receive-result channel)))

(defmacro with-connected-thread (() &body body)
  "Wrap BODY in a lambda and invoke CALL-WITH-CONNECTED-THREAD.
*PGJ-KERNEL* must be bound to the result of MAKE-PGJ-KERNEL."
  `(call-with-connected-thread (lambda () ,@body)))

;;; Non blocking functions and macro

(defun submit-pgj-function (function)
  "Submit the function FUNCTION, with a Postmodern connection, as an
lparallel task on our channel *PGJ-CHANNEL*.  *PGJ-KERNEL* must be
bound to the result of MAKE-PGJ-KERNEL."
  (let ((lparallel:*kernel* *pgj-kernel*))
    (lparallel:task-handler-bind ((error 'lparallel:invoke-transfer-error))
      (lparallel:submit-task *pgj-channel* (make-pgj-task function)))))

(defmacro submit-pgj-task (() &body body)
  "Wrap BODY in a lambda and call SUBMIT-PGJ-FUNCTION.
*PGJ-KERNEL* must be bound to the result of MAKE-PGJ-KERNEL."
  `(submit-pgj-function (lambda () ,@body)))

(defun receive-pgj-result ()
  "Call lparallel:receive-result on our *PGJ-CHANNEL*.
*PGJ-KERNEL* must be bound to the result of MAKE-PGJ-KERNEL."
  (let ((lparallel:*kernel* *pgj-kernel*))
    (lparallel:receive-result *pgj-channel*)))

(defun try-receive-pgj-result (&key timeout)
  "Call lparallel:try-receive-result on our *PGJ-CHANNEL*,
with timeout TIMEOUT, a real.  *PGJ-KERNEL* must be bound to the
result of MAKE-PGJ-KERNEL."
  (let ((lparallel:*kernel* *pgj-kernel*))
    (lparallel:try-receive-result *pgj-channel* :timeout timeout)))
