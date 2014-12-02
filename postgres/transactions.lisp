(in-package :postgres-json)

;;;; Transaction handling

;;; Guidelines gleaned from PostgreSQL 9.4 doco and our use case of
;;; keeping history of all rows:

;;; 'read committed' is the Postgres default isolation level but I'm
;;; setting it explicitly where needed for the avoidance of doubt

;;; set read only on transactions where possible

;;; We use: 1) select by id, 2) insert into -old, 3) update/delete for
;;; the update and delete operations in our model.  I think using the
;;; repeatable read isolation level (rather than serializable) is all
;;; that is required (certainly read committed will not do due to the
;;; multiple steps required).  In some sense _not_ using the
;;; serializable isolation level is a premature optimization, but for
;;; now I am prepared to wear that.  YMMV.

;;; By abuse of notation I am referring to the combination of
;;; isolation level _and_ read only/read write settings as just
;;; 'isolation level'.  For calculating congruence of nested
;;; transactions, the true isolation level is all that matters...

(defprepared serializable-rw
    (:raw "set transaction isolation level serializable read write"))

(defprepared repeatable-read-rw
    (:raw "set transaction isolation level repeatable read read write"))

(defprepared read-committed-ro
    (:raw "set transaction isolation level read committed read only"))

(defprepared read-committed-rw
    (:raw "set transaction isolation level read committed read write"))

;; This should be thread safe as it it only ever bound local to a
;; specific thread.
(defvar *top-isolation-level* nil
  "When we start the first transaction in a nested group, bind this
to the isolation level requested.")

;; I suspect it only makes sense to have a single *-ro setting in this
;; list, and it should be first...
(defvar *isolation-levels-hierarchy*
  '(read-committed-ro read-committed-rw repeatable-read-rw serializable-rw)
  "A list of symbols for functions which set Postgres isolation
levels.  Nested transactions must be started with an isolation level
to the left of, or at the same level as, the top level in the nested
group.")

(defun potential-serialization-failure-p (isolation-level)
  "Certain isolation levels require client handling handling of the
cl-postgres-error:serialization-failure conditio: for example see
with-retry-serialization-failure.  This function returns true if
ISOLATION-LEVEL, a symbol, is such an isolation level."
  (member isolation-level '(repeatable-read-rw serializable-rw)))

(defun isolation-level-position (isolation-level)
  "What POSITION does ISOLATION-LEVEL, a symbol, hold in
*ISOLATION-LEVELS-HIERARCHY*?"
  (position isolation-level *isolation-levels-hierarchy*))

(defun nestable-isolation-level-p (isolation-level)
  "Can you nest a new (logical) transaction with ISOLATION-LEVEL, a
symbol, inside a (true) transaction with *TOP-ISOLATION-LEVEL*?"
  (<= (isolation-level-position isolation-level)
      (isolation-level-position *top-isolation-level*)))

(defmacro ensure-transaction-level ((label isolation-level) &body body)
  "If a transaction is already in progress check that ISOLATION-LEVEL
is congruent with the original transaction and evaluate BODY.
Otherwise wrap BODY forms in a Postmodern WITH-TRANSACTION form and
then call ISOLATION-LEVEL, a symbol for a function that sets the
required isolation level and read only/read write options.  Clients
are responsible for ensuring REPEATEABLE-READ-RW is the minimum
isolation level set for mutating model interface functions such as
INSERT/UPDATE/DELETE.  For REPEATEABLE-READ-RW or SERIALIZABLE-RW,
BODY is wrapped in WITH-RETRY-SERILAIZATION-FAILURE which does what it
says on the tin.  The symbol LABEL will be used to label logging
output.  The result of evaluating the BODY forms is returned."
  (with-unique-names (body-fn tran-fn)
    `(flet ((,body-fn () ,@body))
       (if *top-isolation-level*
           (progn
             (unless (nestable-isolation-level-p ',isolation-level)
               (error 'incompatible-transaction-setting
                      :transaction-name ',label
                      :original *top-isolation-level*
                      :current ',isolation-level))
             (log:trace "Nesting logical transaction ~A" ',label)
             (,body-fn))
           (flet ((,tran-fn ()
                    (log:debug "Starting transaction ~A" ',label)
                    (with-transaction ()
                      (,isolation-level)
                      (let ((*top-isolation-level* ',isolation-level))
                        (multiple-value-prog1 (,body-fn)
                          (log:debug "Completing transaction ~A" ',label))))))
             ,(if (potential-serialization-failure-p isolation-level)
                  `(with-retry-serialization-failure (,label)
                     (,tran-fn))
                  `(,tran-fn)))))))

(defmacro with-model-transaction ((&optional (name 'user)) &body body)
  "Evaluate BODY inside a Postgres transaction using the 'repeatable
read' isolation level.  Nested expansions of this macro or other
transaction macros used internally by POSTGRES-JSON are merely logical
within BODY --- they do not make real Postgres transactions.  So this
macro is designed for bulk inserts or when Atomicity of multiple
inserts/updates/deletes is required."
  `(ensure-transaction-level (,name repeatable-read-rw)
     ,@body))

(defmacro with-retry-serialization-failure ((label) &body body)
  "If *DB-HANDLE-SERIALIZATION-FAILURE-P* is NIL at run time this is a
no op.  Otherwise we wrap BODY forms in a short loop of increasing
sleep times to retry upon seeing
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE conditions from Postmodern.
This condition must be handled when using the PostgreSQL repeatable
read isolation level however we don't sleep more than around 20
seconds in total (but see *SERIALIZATION-FAILURE-SLEEP-TIMES*) so the
end client may _still_ have to handle this condition.  The symbol
LABEL will be used to label logging output.  The result of evaluating
the BODY forms is returned."
  (with-unique-names (body-fn)
    `(flet ((,body-fn () ,@body))
       (if *db-handle-serialization-failure-p*
           (dolist (sleep *serialization-failure-sleep-times* (,body-fn))
             (log:trace "Trying tran loop with sleep: ~A" sleep)
             (handler-case
                 (return (,body-fn))
               (cl-postgres-error:serialization-failure ()
                 (log:debug "Handle serialization failure of ~A.  Sleeping around: ~A" ',label sleep)
                 (unless (zerop sleep) ; Do not wait the first time through
                   (sleep (+ sleep (/ (random 2000) 1000)))))))
           (,body-fn)))))
