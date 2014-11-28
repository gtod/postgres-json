(in-package :postgres-json)

;;;; Model parameters

;;; These serve two purposes:
;;; 1. They are the default values, unless the user overrides them
;;;    when invoking MAKE-MODEL-PARAMETERS, for CREATE-MODEL.
;;; 2. We rebind them (see ENSURE-MODEL-QUERY-OP) before creating
;;;    prepared queries to use against backend model tables.

(defvar *sequence* 'pgj-seq
  "A symbol being the default name of the PostgreSQL sequence to
provide unique IDs for JSON objects inserted into the PostgreSQL
backend base table.")

(defvar *id* 'id
  "A symbol being the name of the primary key column in backend
tables.")

(defvar *id-type* 'integer
  "A symbol being the type of the primary key column in backend
tables.")

(defvar *jdoc* 'jdoc
  "A symbol being the name of the JSON column in created backend
tables.")

(defvar *jdoc-type* 'jsonb
  "A symbol being the type of the JSON column in created backend
tables.")

;;;; Model derived parameters

;;; We bind these in ENSURE-MODEL-QUERY-OP for use by the various
;;; make-<query$> functions.  It may be better to stick the above
;;; specials in some CLOS object and then make these methods but this
;;; is less verbose while we have just a few...

(defvar *table* nil
  "Qualified name of the base table in the model backend.")

(defvar *table-old* nil
  "Qualified name of the old table in the model backend.")

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

;;;; Meta model parameters

(defun meta-model-parameters ()
  "Eating our own dog food, we keep user model parameters in a 'meta'
model, which itself has the following parameters."
  (make-model-parameters :id 'model :id-type 'text))
