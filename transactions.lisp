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
;;; multiple steps required.  In some sense _not_ using the
;;; serializable isolation level is a premature optimization, but for
;;; now I am prepared to wear that.  YMMV.

(in-package :postgres-json)

;;; If you use the repeatable read isolation level you *must* handle
;;; the cl-postgres-error:serialization-failure condition: see
;;; with-retry-serialization-failure
(defprepared repeatable-read-rw
    (:raw "set transaction isolation level repeatable read read write"))

(defprepared read-committed-ro
    (:raw "set transaction isolation level read committed read only"))

(defprepared read-committed-rw
    (:raw "set transaction isolation level read committed read write"))

(defmacro with-transaction-type ((type) &body body)
  "Wrap BODY forms in Postmodern WITH-TRANSACTION and then call TYPE,
a symbol for a function that sets the required isolation level and
read only/read write options."
  `(with-transaction ()
     (,type)
     ,@body))

(defmacro with-retry-serialization-failure ((label) &body body)
  "Wrap BODY forms in a short loop of increasing sleep times to retry
upon seeing CL-POSTGRES-ERROR:SERIALIZATION-FAILURE conditions from
Postmodern.  This condition must be handled when using the PostgreSQL
repeatable read isolation level however we don't sleep more than
around 20 seconds in total so the end client may _still_ have to
handle this condition.  The string LABEL will be used to
label logging output.  The result of evaluating the BODY forms is
returned."
  (with-unique-names (body-fn)
    `(flet ((,body-fn () ,@body))
       (dolist (sleep '(0 1 2 4 7) (,body-fn))
         (handler-case
             (return (,body-fn))
           (cl-postgres-error:serialization-failure ()
             (log:debug "Handle serialization failure of ~A.  Sleeping around: ~A" ,label sleep)
             (unless (zerop sleep) ; Do not wait the first time through
               (sleep (+ sleep (/ (random 2000) 1000))))))))))
