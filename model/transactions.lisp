(in-package :postgres-json)

;;;; Postgres-JSON internal and user transaction handling
;;;; Depends on postgres/transactions.lisp

;;; Guidelines for transaction handling of the mode interface
;;; functions gleaned from the Postgres 9.4 doco:

;;; 'read committed' is the Postgres default isolation level but is
;;; set explicitly by model functions for clarity.

;;; We set 'read only' on transactions where possible.

;;; The model interface functions use: 1) select by key, 2) insert
;;; into -old, 3) update or delete for their update and excise
;;; operations.  AFAIK using the repeatable read isolation level
;;; (rather than serializable) should suffice.  Certainly read
;;; committed is insufficent due to the multiple steps required for
;;; maintaining history rows.

(defun call-with-maybe-transaction (label isolation-level thunk)
  (if *top-isolation-level*
      (call-with-ensured-transaction-level label isolation-level thunk)
      (if (potential-serialization-failure-p isolation-level)
          (call-with-retry-serialization-failure
           label
           (lambda ()
             (call-with-transaction-level label isolation-level thunk)))
          (call-with-transaction-level label isolation-level thunk))))

(defun call-with-model-transaction (label isolation-level thunk)
  (if *top-isolation-level*
      (call-with-logical-transaction-level label isolation-level thunk)
      (call-with-retry-serialization-failure
       label
       (lambda ()
         (call-with-transaction-level label isolation-level thunk)))))

;;;; Postrgre-JSON model interface function use

(defmacro maybe-transaction ((name isolation-level) &body body)
  "Just a wrapper around an ensured transaction using ISOLATION-LEVEL
and with retry for serialization failures.  NAME is used only for
logging."
  `(call-with-maybe-transaction ',name ',isolation-level
                                ,(transaction-thunk name body)))

;;;; Public

(defmacro with-model-transaction ((&optional name) &body body)
  "Evaluate BODY inside a Postgres transaction using the 'repeatable
read' isolation level in read/write mode.  Retry any serialization
failures although chronic incidence will still result in the client
seeing CL-POSTGRES-ERROR:SERIALIZATION-FAILURE conditions --- see also
*SERIALIZATION-FAILURE-SLEEP-TIMES*.  Implemented using Postmodern
WITH-LOGICAL-TRANSACTION so may be nested.  NAME can be used with
Postmodern's abort-transaction and commit-transaction. NAME should not
be a Postgres reserved word.  Ideal for any group of mutating model
interface functions."
  (let ((name (or name (gensym "TRAN"))))
    `(call-with-model-transaction ',name ',*pgj-default-isolation-level*
                                  ,(transaction-thunk name body))))

(defun rollback (name)
  "If this is the root node of a nested set of WITH-MODEL-TRANSACTIONs
then 'rollback' the transaction NAME.  Otherwise rollback to the
Postgres savepoint NAME."
  (pomo:abort-logical-transaction name))

(defun commit (name)
  "If this is the root node of a nested set of WITH-MODEL-TRANSACTIONs
then 'commit' the transaction NAME.  Otherwise merely release the
savepoint NAME."
  (pomo:commit-logical-transaction name))
