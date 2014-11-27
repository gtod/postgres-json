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

;; Investigate calling deallocate to drop prepared queries
;; But they are probably connection specific anyway...
;; What are the consequences of that?
(defun bake-model (model &key (schema *db-schema*) (sequence *db-sequence*))
  "Prepare all the PostgreSQL queries necessary to support PostgreSQL
JSON persistence model calls on MODEL, a symbol.  SCHEMA and SEQUENCE,
both symbols, may be specified to use a specific DB schema and/or
sequence for primary keys, respectively."
  (let* ((base model)
         (old (sym t base "-old"))
         (table (qualified-name base schema))
         (table-old (qualified-name old schema)))
    (make-nextval-sequence$ model (qualified-name-string sequence schema))
    (make-insert$ model table)
    (make-insert-old$ model table table-old)
    (make-update$ model table)
    (make-get$ model table)
    (make-delete$ model table)
    (make-keys$ model table)
    model))

;; (defun drop-backend (name))
;; (defun delete-model (name))

;;;; SEMI PUBLIC


