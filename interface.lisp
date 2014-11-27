(in-package :postgres-json)

;;;; PUBLIC

(defvar *db-schema* 'pgj-schema
  "A symbol being the name of the default PostgreSQL schema to use to
house database objects.")

(defvar *db-sequence* 'pgj-seq
  "A symbol being the name of the default global PostgreSQL sequence
that will be created to provide unique IDs for all JSON objects
inserted into PostgreSQL backend model tables by this library.")

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

(defun create-model-backend (model &key (schema *db-schema*))
  "Create PostgreSQL tables and other DB objects with names based on
MODEL, a symbol, for a PostgreSQL JSON persistence model.  Create the
DB objects in database schema *DB-SCHEMA*.  This should only be run
once per model you wish to create, typically at the REPL."
  (let* ((name model)
         (name-old (sym t name "-old"))
         (index (sym t name "-gin"))
         (index-old (sym t name "-old-gin")))
    (create-base-table name schema)
    (create-old-table name-old schema)
    (when (eq 'jsonb *jdoc-type*)
      (create-gin-index index name schema)
      (create-gin-index index-old name-old schema))))

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


