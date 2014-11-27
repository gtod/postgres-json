(in-package :postgres-json)

;;;; PUBLIC

(defvar *to-json* 'to-json
  "Function designator for function of one argument to serialize lisp
objects (submitted to INSERT and UPDATE, for example) to JSON.  Bind
it at run time for use by the model interface functions.  Or redefine
it globally for use in your own project.")

(defvar *from-json* 'yason:parse
  "Function designator for function of one argument to make lisp
objects from JSON strings retrieved from the DB backend.  Used by GET,
for example.  Bind it at run time for use by the model interface
functions.  Or redefine it globally for use in your own project.")

(defvar *db-handle-serialization-failure-p* t
  "UPDATE and DELETE calls on the model will use the Postgres
'repeatable read isolation level' so 'serialization failures' may
occur.  When this special variable is set to T (the default), these
failures are handled under the covers.  (However, if excessive time
elapses, client code may still see a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE).  If you would rather
explicitly handle _all_ serialization failures in your client code,
set this to NIL.")

;; I think it sort of makes sense not to sleep at all for the first
;; retry, but then to back off pretty fast.  But I am no expert...
(defvar *serialization-failure-sleep-times* '(0 1 2 4 7)
  "The length of this list of real numbers determines the number of
times to retry when a Postgres transaction COMMIT see a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE condition.  For each retry we
sleep the duration specified, plus a random number of milliseconds
between 0 and 2000.  However, if 0 sleep is specified, we do not sleep
at all.")

;; I spent a long time letting users create and user arbitrary schema.
;; It's much simpler (for now) not too, and not to let them use the
;; public schema either...
(defun create-backend ()
  "Create the schema *PGJ-SCHEMA* and other backend objects needed to
house user created PostgreSQL JSON persistence models.  Call just once
in a given PostgreSQL database."
  (if (pomo:schema-exist-p *pgj-schema*)
      (error 'database-safety-net
             :attempted-to (format nil "Create the backend, when a schema called ~A already exists." *pgj-schema*)
             :suggestion (format nil "Check carefully what data is in ~A" *pgj-schema*))
      (progn
        (pomo:create-schema *pgj-schema*)
        (create-db-sequence *pgj-sequence* *pgj-schema*)
        (create-model *meta-model* (meta-model-parameters))
        *pgj-schema*)))

(defun create-model (model &optional (parameters (make-model-parameters)))
  "Create the PostgreSQL tables and indexes for PostgreSQL JSON
persistence model MODEL, a symbol.  This should only be called once
per model.  Returns MODEL."
  (flet ((param (key)
           (gethash key parameters)))
    ;; We could, of course, pass the parameters hash around instead...
    (let ((*id* (param "id"))
          (*id-type* (param "id-type"))
          (*jdoc* (param "jdoc"))
          (*jdoc-type* (param "jdoc-type")))
      (let* ((schema *pgj-schema*)
             (name model)
             (name-old (sym t name "-old"))
             (index (sym t name "-gin"))
             (index-old (sym t name "-old-gin")))
        (create-base-table name schema)
        (create-old-table name-old schema)
        (when (eq 'jsonb *jdoc-type*)
          (create-gin-index index name schema)
          (create-gin-index index-old name-old schema))
        (unless (eq *meta-model* model)
          (insert *meta-model* (maphash-symbols-to-strings parameters)
                  :use-id (symbol-name model)))
        model))))

(defun make-model-parameters (&key (sequence *pgj-sequence*) (id *id*) (id-type *id-type*)
                                   (jdoc *jdoc*) (jdoc-type *jdoc-type*))
  "Create a hash-table of parameters to specify backend features of a
PostgreSQL JSON persistence model, typically to be supplied to
CREATE-MODEL.  For each keyword argument see the documentation of the
special variable that is its default value."
  (obj "sequence" sequence
       "id" id
       "id-type" id-type
       "jdoc" jdoc
       "jdoc-type" jdoc-type))

(defun drop-backend! ()
  "Drop the backend (that is the PostgreSQL schema *PGJ-SCHEMA*) in
the database Postmodern is currently connected to.  This will
irrevocably delete ALL your data in ALL your models so it uses
a RESTART-CASE toguard against human error."
  (flet ((drop ()
           (drop-db-schema-cascade *pgj-schema*)))
    (let ((attempted-to (format nil "DROP all models' data(!) in schema: ~A" *pgj-schema*)))
      (restart-case (error 'database-safety-net
                           :attempted-to attempted-to
                           :suggestion "Pick an appropriate restart")
        (cancel () :report "Leave this schema alone." (return-from drop-backend! nil))
        (really-do-it () :report "I really want to drop ALL data in ALL models(!)" (drop))))))

(defun drop-model! (model)
  "Drop model MODEL.  This will irrevocably delete all data associated
with the model so it uses a RESTART-CASE to guard against human
error."
  (flet ((drop ()
           ;; Would be nice if these three were in one transaction...
           (delete *meta-model* (symbol-name model))
           (drop-db-table-cascade model *pgj-schema*)
           (drop-db-table-cascade (sym t model "-old") *pgj-schema*)))
    (restart-case (error 'database-safety-net
                         :attempted-to (format nil "DROP model ~A" model)
                         :suggestion "Pick an appropriate restart")
      (cancel () :report "Leave this model alone." (return-from drop-model! nil))
      (really-do-it () :report "I really want to DROP this model" (drop)))))

(defun flush-prepared-queries ()
  "If you get a 'Database error 26000: prepared statement ... does not
exist error' while mucking around at the REPL, call this.  A similar
error in production code should be investigated."
  (setf *query-functions* (make-hash-table :test #'equal)))
