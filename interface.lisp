(in-package :postgres-json)

;;;; PUBLIC

;;; We have two validity periods for our public special variables:
;;; 'bake' time and run time.

;;; If a special variable has bake time validity, its value will be
;;; used *only at macro expansion time*, to determine aspects of the
;;; model.  It may also be used by DB functions unrelated to specific
;;; models, such as CREATE-DEFAULT_SCHEMA.

;;; Run time validity means that the special can be rebound by the
;;; user for any specific model call --- it is actually evaluated in
;;; the run time code the baking macros generate.

;;; It is well worth macroexpanding a (bake-interface cat) call and
;;; then macroexpanding down into say (defun-update ...) and further
;;; to get all this mubo jumbo to gell.

(defvar *db-schema* 'pgj-schema
  "A symbol being the name of the default PostgreSQL schema to use to
house our database objects.  This is not a run time special variable:
it is only used by CREATE-DEFAULT-SCHEMA, CREATE-DEFAULT-SEQUENCE,
CREATE-BACKEND and BAKE-INTERFACE.")

(defvar *db-sequence* 'pgj-seq
  "A symbol being the name of the default global PostgreSQL sequence
that will be created to provide unique IDs for all JSON objects
inserted into PostgreSQL tables by this library.  This is not a run
time special variable: it is only used by CREATE-DEFAULT-SEQUENCE,
CREATE-BACKEND and BAKE-INTERFACE.")

(defvar *db-handle-serialization-failure-p* t
  "UPDATE and DELETE calls on the model will use the Postgres
'repeatable read isolation level' so 'serialization failures' may
occur.  When this special variable is set to T (the default), these
failures are handled under the covers.  (However, if excessive time
elapses, client code may still see a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE).  If you would rather
explicitly handle _all_ serialization failures in your client code,
set this to NIL.  This is a run time variable, that is the model code
respects this setting for any specifc model UPDATE or DELETE call.")

;; I think it sort of makes sense not to sleep at all for the first
;; retry, but then to back off pretty fast.  But I am no expert...
(defvar *serialization-failure-sleep-times* '(0 1 2 4 7)
  "The length of this list of real numbers determines the number of
times to retry when a Postgres transaction COMMIT see a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE condition.  For each retry we
sleep the duration specified, plus a random number of milliseconds
between 0 and 2000.  However, if 0 sleep is specified, we do not sleep
at all.  This is a run time variable, that is the model code respects
this setting for any specifc model UPDATE or DELETE call.")

;;; Before everything - create sequence, create schema
(defun create-default-schema ()
  "Create a PostgreSQL schema with name *DB-SCHEMA*."
  (create-schema *db-schema*))

(defun create-default-sequence ()
  "Create a PostgreSQL sequence with name *DB-SEQUENCE* in *DB-SCHEMA*
to act as the source of unique ids across _all_ database model tables.
If using PSQL remember to do something like

  SET search_path TO pgj_schema, public;

to be able to see the tables, indexes, etc. in your new schema."
  (create-sequence *db-sequence* *db-schema*))

;; Want to change this to model...
;;; Once only per model, typically at the REPL
(defmacro create-backend (name)
  "Create PostgreSQL tables and other objects with names based on
NAME, a symbol and for a PostgreSQL JSON persistence model lisp
package, also called NAME.  Create the DB objects in database schema
*DB-SCHEMA*.  This should only be run once per model you wish to
create, typically at the REPL."
  (check-type name symbol)
  (let* ((schema *db-schema*)
         (name-old (sym t name "-old"))
         (index (sym t name "-gin"))
         (index-old (sym t name "-old-gin")))
    `(progn
       (create-base-table ',name ',schema)
       (create-old-table ',name-old ',schema)
       (when (eq 'jsonb *jdoc-type*)
         (create-gin-index ',index ',name ',schema)
         (create-gin-index ',index-old ',name-old ',schema)))))

;; There is an important serial dependence of the various definitions
;; of our model, found here in BAKE-INTERFACE.  Consider an example
;; model with symbol cat. The _definition_ of model interface function
;; cat:insert can refer to model implementation function cat::insert$
;; as just plain insert$ (see defun-insert) in the simple sense that
;; the macroexpansion of insert$ below occurs _before_ that of insert.
;; But it will not work the other way around!  That is, you cannot
;; forward reference model functions.  Fortunately you will get
;; undefined function warnings if you try it.  It's all to do with the
;; dynamic determining of what's currently fbound inside the model by
;; macro with-fbound-symbols-in-package.  If I knew a better way to do
;; it, I'd use it...

;; I think I want to change this to (model &optional (package-name model))
(defmacro bake-interface (name &key (to-json 'to-json))
  "Dynamically create (or recreate) and populate a lisp package called
NAME, a symbol, to house the implementation and public interface
functions of a PostgreSQL JSON persistence model.  Export the symbols
of the interface functions from that package.  Once 'baked' all the
schema, sequence, table names etc. are hardcoded into Postmodern
prepared queries, so can not be modified at run time.  TO-JSON may be
a symbol for any function of one argument that will serialize lisp
objects to JSON.  You must have previously invoked CREATE-BACKEND for
a model of the same name and using the same values for *DB-SCHEMA* and
*DB-SEQUENCE*.  Returns the model package."
  (ensure-model-package name)
  (let* ((schema *db-schema*)
         (name-old (sym t name "-old"))
         (table (qualified-name name schema))
         (table-old (qualified-name name-old schema))
         (next-id (sequence-op-name "nextval" *db-sequence* schema name)))
    `(progn
       ;; Low level DB access
       (defprepare-nextval-sequence$ ,*db-sequence* ,schema ,name)
       (defprepare-insert$ ,name ,table)
       (defprepare-insert-old$ ,name ,table ,table-old)
       (defprepare-update$ ,name ,table)
       (defprepare-get$ ,name ,table)
       (defprepare-delete$ ,name ,table)
       (defprepare-get-all-ids$ ,name ,table)

       ;; Exported model functions proper
       (defun-insert ,name :next-id ,next-id :to-json-fn ,to-json)
       (defun-update ,name :to-json-fn ,to-json)
       (defun-get ,name)
       (defun-delete ,name)
       (defun-keys ,name)

       (find-package ',name))))

;; (defun drop-backend (name))
;; (defun delete-model (name))

;;;; SEMI PUBLIC

(defun obj (&rest args)
  "Return an 'equal hash-table consisting of pairs of ARGS."
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (key val) on args by #'cddr do
      (setf (gethash key hash) val))
    hash))

(defun pp-json (object &key (stream *terminal-io*) (indent 4))
  "Pretty print lisp OBJECT as JSON to stream with specified indent."
  (let ((s (yason:make-json-output-stream stream :indent indent)))
    (yason:encode object s)))

(defun to-json (object)
  "Convert a lisp OBJECT to a string of JSON."
  (with-output-to-string (s)
    (yason:encode object s)))

(defun from-json (json)
  "Convert a JSON string to a lisp object."
  (yason:parse json))

(defun stash-id (id hash)
  "Add the pair \"id\" => ID to the hash-table HASH."
  (let ((copy (copy-hash-table hash)))
    (setf (gethash "id" copy) id)
    copy))
