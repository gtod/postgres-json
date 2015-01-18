(in-package :postgres-json)

;;;; Postmodern transactions with isolation levels and RO, RW
;;;; settings plus serialization failure handling.

;;; The following adds support for setting the isolation level of a
;;; Postmodern transaction, and setting the read only or read write
;;; status of that transaction at the same time. See
;;; http://www.postgresql.org/docs/9.4/static/sql-set-transaction.html

;;; There is also support for a rudimentary retry loop to catch the
;;; cl-postgres-error:serialization-failure conditions that may arise
;;; when using 'repeatable read' or 'serializable' isolation levels.
;;; Since 'read committed' is the default isolation level, many
;;; Postgres users will never have seen such failures.  Postgres-JSON
;;; requires either of the above isolation levels because of, say,
;;; SUPERSEDE keeping a full history under the covers...

;;; A small change to Postmodern is required to support these
;;; additions, see postgres/postmodern.lisp

;;;; Public specials

(define-constant serializable-rw
  "isolation level serializable read write" :test 'string=
  :documentation "START TRANSACTION string to set Postgres
'Serializable' isolation level and read/write.")

(define-constant repeatable-read-rw
  "isolation level repeatable read read write" :test 'string=
  :documentation "START TRANSACTION string to set Postgres 'Repeatable
read' isolation level and read/write."  )

(define-constant read-committed-ro
  "isolation level read committed read only" :test 'string=
  :documentation "START TRANSACTION string to set Postgres 'Read
committed' isolation level, which is the default, and read only.")

(define-constant read-committed-rw
  "isolation level read committed read write" :test 'string=
  :documentation "START TRANSACTION string to set Postgres 'Read
committed' isolation level, which is the default, and read write.")

(defvar *pgj-default-isolation-level* 'repeatable-read-rw
  "The isolation level to use for WITH-MODEL-TRANSACTION.  For models
that maintain history can only be REPEATABLE-READ-RW or
SERIALIZABLE-RW.  For models without history could conceivably be
READ-COMMITTED-RW.")

;;;; Implementation

;; This should be thread safe as it it only ever bound local to a
;; specific thread.
(defvar *top-isolation-level* nil
  "When we start the first transaction in a nested group, bind this
to the isolation level requested.")

;; By abuse of notation I am referring to the combination of isolation
;; level _and_ read only/read write settings as just 'isolation
;; level'.  For calculating the congruence of nested transactions the
;; true isolation level is paramount but clearly you can't nest a RW
;; level inside a RO one...
(defvar *isolation-levels-hierarchy*
  '(read-committed-ro read-committed-rw repeatable-read-rw serializable-rw)
  "A list of symbols for string constants which set Postgres isolation
levels, and read only or read/write settings.  Nested transactions must
be started with an isolation level to the left of, or at the same
level as, the top level in the nested group.")

(defun potential-serialization-failure-p (isolation-level)
  "Certain isolation levels require client handling of the
cl-postgres-error:serialization-failure condition.  This function
returns true if ISOLATION-LEVEL, a symbol, is such an isolation
level."
  (member isolation-level '(repeatable-read-rw serializable-rw)))

(defun isolation-level-position (isolation-level)
  "What POSITION does ISOLATION-LEVEL, a symbol, hold in the
*ISOLATION-LEVELS-HIERARCHY*?"
  (position isolation-level *isolation-levels-hierarchy*))

(defun nestable-isolation-level-p (top-isolation-level isolation-level)
  "Can you nest a new (virtual) transaction with ISOLATION-LEVEL, a
symbol, inside a (true) transaction with TOP-ISOLATION-LEVEL?"
  (<= (isolation-level-position isolation-level)
      (isolation-level-position top-isolation-level)))

(define-condition incompatible-transaction-setting (error)
  ((transaction-name :initarg :transaction-name :reader transaction-name)
   (original :initarg :original :reader original)
   (current :initarg :current :reader current))
  (:report (lambda (condition stream)
             (format stream "You cannot nest the transaction named ~A with isolation level ~A
inside a transaction with isolation level ~A."
                     (transaction-name condition)
                     (current condition)
                     (original condition))))
  (:documentation "Signaled for a nested invocation of
WITH-ENSURED-TRANSACTION-LEVEL or WITH-LOGICAL-TRANSACTION-LEVEL
inside a previous invocation with an incongruent isolation level."))

(defun check-isolation-level (label isolation-level)
  (unless (nestable-isolation-level-p *top-isolation-level* isolation-level)
    (error 'incompatible-transaction-setting
           :transaction-name label
           :original *top-isolation-level*
           :current isolation-level)))

(defun transaction-thunk (transaction body)
  `(lambda (,transaction)
    (declare (ignorable ,transaction))
    ,@body))

;;;; Transactions proper in CALL-WITH style.
;;;; Semi public - some clients may need/want these

(defun call-with-transaction-level (label isolation-level thunk)
  (log:debug "Starting transaction ~A" label)
  ;; See postgres/postmodern.lisp for our proposed mods to Postmodern
  (let ((pomo:*transaction-mode* (symbol-value isolation-level))
        (*top-isolation-level* isolation-level))
    (multiple-value-prog1 (pomo:call-with-transaction thunk)
      (log:debug "Completing transaction ~A" label))))

(defun call-with-logical-transaction-level (label isolation-level thunk)
  (multiple-value-prog1
      (if *top-isolation-level*
          (progn
            (check-isolation-level label isolation-level)
            (log:debug "Nesting logical transaction ~A" label)
            (pomo:call-with-logical-transaction label thunk))
          (call-with-transaction-level label isolation-level thunk))
    (log:debug "Completing logical transaction ~A" label)))

(defun call-with-ensured-transaction-level (label isolation-level thunk)
  (multiple-value-prog1
      (if *top-isolation-level*
          (progn
            (check-isolation-level label isolation-level)
            (log:trace "Nesting ensured transaction ~A" label)
            ;; There is no "real" transaction to abort, so don't pass in label
            (funcall thunk nil))
          (progn
            (log:trace "Starting ensured transaction ~A" label)
            (call-with-transaction-level label isolation-level thunk)))
    (log:trace "Completing ensured transaction ~A" label)))

(defun call-with-retry-serialization-failure (label thunk)
  (if *serialization-failure-sleep-times*
      (dolist (sleep *serialization-failure-sleep-times* (funcall thunk))
        (log:trace "In retry serial loop with sleep: ~A" sleep)
        (handler-case
            (return (funcall thunk))
          (cl-postgres-error:serialization-failure ()
            (log:debug "Handle serialization failure of ~A.  Sleeping around: ~A" label sleep)
            (unless (zerop sleep) ; Do not wait the first time through
              (sleep (+ sleep (/ (random 2000) 1000)))))))
      (funcall thunk)))

;;;; Public macro interface

;;; These have the same name as their Postmodern counterparts but with
;;; a -level suffix.  The do the same thing but allow specification of
;;; an isolation level.

(defmacro with-transaction-level ((name isolation-level) &body body)
  "Unilaterally evaluate BODY inside a Postmodern WITH-TRANSACTION
form with Postgres 'transaction mode' set to the symbol-value of
ISOLATION-LEVEL, a symbol.  The symbol NAME is bound to the Postmodern
`transaction-handle' and may be used in calls to Postmodern's
abort-transaction and commit-transaction."
  `(call-with-transaction-level ',name ',isolation-level
                                ,(transaction-thunk name body)))

(defmacro with-logical-transaction-level ((name isolation-level) &body body)
  "Similar to Postmodern's WITH-LOGICAL-TRANSACTION but start any top
level transaction with Postgres 'transaction mode' set to the
symbol-value of ISOLATION-LEVEL.  The symbol NAME is bound to the
Postmodern `transaction-handle' and may be used in calls to
Postmodern's abort-transaction and commit-transaction.  The condition
`incompatible-transaction-setting' will be signaled for incongruent
nested isolation levels."
  `(call-with-logical-transaction-level ',name ',isolation-level
                                        ,(transaction-thunk name body)))

(defmacro ensure-transaction-level ((isolation-level) &body body)
  "Similar to Postmodern's ENSURE-TRANSACTION but start any top level
transaction with Postgres 'transaction mode' set to the symbol-value
of ISOLATION-LEVEL.  The condition `incompatible-transaction-setting'
will be signaled for incongruent nested isolation levels."
  (let ((label (gensym "TRAN")))
    `(call-with-ensured-transaction-level ',label ',isolation-level
                                          ,(transaction-thunk label body))))
