(in-package :postgres-json)

;;;; Transaction handling

;;; Guidelines gleaned from PostgreSQL 9.4 doco and our use case of
;;; keeping history of all rows:

;;; 'read committed' is the default isolation level but I'm setting it
;;; explicitly where needed for the avoidance of doubt

;;; set read only on transactions where possible

;;; We use: 1) select by id, 2) insert into -old, 3) update/delete for
;;; the update and delete operations in our model.  I think using the
;;; repeatable read isolation level (rather than serializable) is all
;;; that is required (certainly read committed will not do due to the
;;; multiple steps required).  In some sense _not_ using the
;;; serializable isolation level is a premature optimization, but for
;;; now I am prepared to wear that.  YMMV.

;;; If you use the repeatable read isolation level you *must* handle
;;; the cl-postgres-error:serialization-failure condition: see
;;; with-retry-serialization-failure
(defprepared repeatable-read-rw
    (:raw "set transaction isolation level repeatable read read write"))

(defprepared read-committed-ro
    (:raw "set transaction isolation level read committed read only"))

(defprepared read-committed-rw
    (:raw "set transaction isolation level read committed read write"))

(defvar *top-level-transaction-settings* nil
  "When we start the first transaction in a nested group, bind this
to the transaction settings requested.")

;; Thread safety?
(defvar *transaction-settings-priority*
  '(read-committed-ro read-committed-rw repeatable-read-rw)
  "A ist of transaction settings symbols.  Nested transactions must be
started with settings to the left of, or at the same level as, the top
level setting in the nested group.")

(defun transaction-settings-level (transaction-setting)
  (position transaction-setting *transaction-settings-priority*))

(defun congruent-transaction (transaction-setting)
  (<= (transaction-settings-level transaction-setting)
      (transaction-settings-level *top-level-transaction-settings*)))

(defmacro ensure-transaction-type ((name type) &body body)
  "If a transaction is already in progress simply evaluate BODY.
Otherwise wrap BODY forms in a Postmodern WITH-TRANSACTION form with
name NAME (a symbol) and then call TYPE, a symbol for a function that
sets the required isolation level and read only/read write options.
Clients are responsible for ensuring REPEATEABLE-READ-RW is the
minimum isolation level set for mutating model interface functions
such as INSERT/UPDATE/DELETE and for ensuring embedded transactions
are congruent with the original isolation level and RO/RW settings."
  (with-unique-names (body-fn)
    `(flet ((,body-fn () ,@body))
       (log:debug "Tran: ~A" ',type)
       (if *top-level-transaction-settings*
           (progn
             (unless (congruent-transaction ',type)
               (error 'incompatible-transaction-setting
                      :transaction-name ',name
                      :original *top-level-transaction-settings*
                      :current ',type))
             (,body-fn))
           (with-transaction (,name) ; What is the purpose of this name in pomo?
             (declare (ignorable ,name))
             (,type)
             (let ((*top-level-transaction-settings* ',type))
               (,body-fn)))))))

(defmacro with-retry-serialization-failure ((label) &body body)
  "If *DB-HANDLE-SERIALIZATION-FAILURE-P* is NIL at run time this is a
no op.  Otherwise we wrap BODY forms in a short loop of increasing
sleep times to retry upon seeing
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE conditions from Postmodern.
This condition must be handled when using the PostgreSQL repeatable
read isolation level however we don't sleep more than around 20
seconds in total (but see *SERIALIZATION-FAILURE-SLEEP-TIMES*) so the
end client may _still_ have to handle this condition.  The string
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
                 (log:debug "Handle serialization failure of ~A.  Sleeping around: ~A" ,label sleep)
                 (unless (zerop sleep) ; Do not wait the first time through
                   (sleep (+ sleep (/ (random 2000) 1000)))))))
           (,body-fn)))))
