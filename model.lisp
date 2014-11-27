(in-package :postgres-json)

;;;; Model backend specials

;;; Thse specials are effectively constants.  You _could_ rebind them
;;; but you would need to do it for every use of every interface
;;; function.  If you do need different values you would be better of
;;; just redefining these in some file in your own project.

(defvar *pgj-schema* 'pgj-model
  "A symbol being the name of the PostgreSQL schema we create to house
all database backend objects.")

;; No reason for user to change this, it sits in a fresh schema we made.
(defvar *pgj-sequence* 'pgj-seq
  "A symbol being the name of the PostgreSQL sequence we create
for (at least) the use of of meta model.")

(defvar *meta-model* 'pgj-meta
  "A symbol being the name of the model in which we store meta data
relating to user models.")

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

;;;; Meta model

(defun meta-model-parameters ()
  "Eating our own dog food, we keep user model parameters in a 'meta'
model, which itself has the following parameters."
  (make-model-parameters :id 'model :id-type 'text))

;;;; Create database backend using model name, a symbol

(defun create-base-table (name schema)
  "Create a PostgreSQL table called NAME in SCHEMA, both symbols.
This is the base table for the model.  Requires an active DB
connection."
  (run `(:create-table ,(qualified-name-string name schema)
         ((,*id*      :type ,*id-type*   :primary-key t)
          (valid-to   :type timestamptz :default (:type "infinity" timestamptz))
          (valid-from :type timestamptz :default (:transaction-timestamp))
          (,*jdoc*    :type ,*jdoc-type*)))))

(defun create-old-table (name schema)
  "Create a PostgreSQL table called NAME in SCHEMA, both symbols.
This is the 'old' table for the model, which will store non current
rows.  Requires an active DB connection."
  (run `(:create-table ,(qualified-name-string name schema)
         ((,*id*      :type ,*id-type*)
          (valid-to   :type timestamptz)
          (valid-from :type timestamptz)
          (,*jdoc*    :type ,*jdoc-type*))
         (:primary-key ,*id* valid-to))))

(defun create-gin-index (name table schema)
  "Create a PostgreSQL GIN index with NAME on a database table TABLE
in SCHEMA, all symbols.  Note that if you use *JDOC-TYPE* 'json when
creating the tables you cannot then create these indexes --- see the
PostgreSQL documentation.  In fact the option 'jsonb_path_ops' to the
GIN index may be desirable under some circumstances but I have not yet
twisted s-sql to generate such an index declaration (clearly we could
just use a string).  Requires an active DB connection."
  (run `(:create-index ,name :on ,(qualified-name-string table schema)
         :using gin :fields ,*jdoc*)))

;;;; Postmodern query management

(defparameter *query-functions* (make-hash-table :test #'equal)
  "Hash of (for example) \"cat:insert$\" => query function.")

(defun query-key (model operation)
  (format nil "~A:~A" (symbol-name model) (symbol-name operation)))

(defun lookup-query (model operation)
  (gethash (query-key model operation) *query-functions*))

(defun set-lookup-query (model operation query)
  (setf (gethash (query-key model operation) *query-functions*)
        query))

(defsetf lookup-query set-lookup-query)

;;;; Make our queries, on demand per model name (a symbol)

(defmacro defun-make-query (name (&rest args) (query &optional (format :rows)))
  `(defun ,(sym t "make-" name) (model ,@args)
     (setf (lookup-query model ',name)
           (prepare (sql-compile ,@(cdr query)) ,format))))

(defun-make-query nextval-sequence$ ()
    ('`(:select (:nextval ,(qualified-name-string *sequence* *pgj-schema*))) :single!))

(defun-make-query insert$ ()
    ('`(:insert-into ,*table* :set ',*id* '$1 ',*jdoc* '$2
                     :returning ',*id*)
     :single!))

(defun-make-query insert-old$ ()
  ('`(:insert-into ,*table-old*
                   ;; Note the dependence on the column ordering of
                   ;; CREATE-OLD-TABLE since :insert-into will not let
                   ;; me explicitly specify column names...
                   (:select ',*id*
                            (:transaction-timestamp)
                            'valid-from
                            ',*jdoc*
                            :from ,*table*
                            :where (:= ',*id* '$1)))))

(defun-make-query update$ ()
    ('`(:update ,*table*
        :set ',*jdoc* '$2 'valid-from (:transaction-timestamp)
        :where (:= ',*id* '$1)
        :returning ',*id*)
     :single))

(defun-make-query get$ ()
    ('`(:select ',*jdoc* :from ,*table* :where (:= ',*id* '$1))
     :single!))

(defun-make-query delete$ ()
    ('`(:delete-from ,*table* :where (:= ',*id* '$1) :returning ',*id*)
     :single))

(defun-make-query keys$ ()
    ('`(:select ',*id* :from ,*table*)
     :column))

;;;; Define a simple interace to any queries generated by the above

(defmacro defun-query (name (&rest args))
  `(defun ,name (model ,@args)
     (funcall (lookup-query model ',name) ,@args)))

(defun-query nextval-sequence$ ())
(defun-query insert$ (id jdoc))
(defun-query insert-old$ (id))
(defun-query update$ (id jdoc))
(defun-query get$ (id))
(defun-query delete$ (id))
(defun-query keys$ ())

;;;; Generate prepared queries on a per model/query basis

(defun ensure-model-query (model &rest operations)
  (dolist (op operations)
    (ensure-model-query-op model op)))

(defun ensure-model-query-op (model operation)
  "If (say) cat:insert$ exists then return that query (a function).
If not we make the query OPERATION on demand by getting the params for
MODEL from the meta model.  Of course, we fix the bootstrap problem by
calling a function to supply the meta model's own parameters."
  (if (lookup-query model operation)
      (log:debug "Using prepared query for ~A:~A" model operation)
      (let ((parameters (if (eq *meta-model* model)
                            (meta-model-parameters)
                            (let ((params (get *meta-model* (symbol-name model))))
                              (maphash-strings-to-symbols params)))))
        (flet ((param (key)
                 (gethash key parameters)))
          (let* ((schema *pgj-schema*)
                 (base model)
                 (old (sym t base "-old"))
                 (*table* (qualified-name base schema))
                 (*table-old* (qualified-name old schema))
                 (*sequence* (param "sequence"))
                 (*id* (param "id"))
                 (*id-type* (param "id-type"))
                 (*jdoc* (param "jdoc"))
                 (*jdoc-type* (param "jdoc-type")))
            (log:debug "Preparing query for ~A:~A" model operation)
            (funcall (sym :postgres-json "make-" operation) model))))))

;;;; Define the model proper

;;; Need to comment on acceptable type of ID: integer, string, ??

(defun insert (model object &key use-id stash-id (to-json *to-json*))
  "Insert lisp object OBJECT into the backend MODEL, a symbol,
after JSON serialization.  If USE-ID is supplied, use that as the
primary key for this object rather than the automatically generated
one.  If STASH-ID is a symbol we FUNCALL it with two arguments: the
value of the id to be used for the DB insert and OBJECT.  TO-JSON must
be a function designator for a function of one argument to serialize
lisp objects to JSON strings.  Return the id."
  (unless use-id
    (ensure-model-query model 'nextval-sequence$))
  (ensure-model-query model 'insert$)
  (with-transaction-type (read-committed-rw)
    (let* ((id (if use-id use-id (nextval-sequence$ model)))
           (object (if stash-id
                       (funcall stash-id id object)
                       object)))
      (nth-value 0 (insert$ model id (funcall to-json object))))))

(defun update (model id object &key (to-json *to-json*))
  "Update the current value of the object with primary key ID (of type
compatible with Postgres type *ID-TYPE*) in backend MODEL, a symbol,
to be the JSON serialization of OBJECT.  TO-JSON must be a function
designator for a function of one argument to serialize lisp objects to
JSON strings.  Returns ID on success, NIL if there was no such ID
found."
  (log:debug "Attempt update of ~A" id)
  (ensure-model-query model 'insert-old$ 'update$)
  (with-retry-serialization-failure ("update")
    (with-transaction-type (repeatable-read-rw)
      (insert-old$ model id)
      (nth-value 0 (update$ model id (funcall to-json object))))))

(defun get (model id &key (from-json *from-json*))
  "Lookup the object with primary key ID (of type compatible with
Postgres type *ID-TYPE*) in MODEL, a symbol, and return a parse of the
JSON string by the the function of one argument designated by
FROM-JSON.  Make it #'identity to return just the JSON string proper."
  (ensure-model-query model 'get$)
  (funcall from-json (with-transaction-type (read-committed-ro)
                       (get$ model id))))

(defun delete (model id)
  "Delete the object with primary key ID (of type compatible with
Postgres type *ID-TYPE*).  Returns ID on success, NIL if there was no
such ID found."
  (log:debug "Attempt delete of ~A" id)
  (ensure-model-query model 'insert-old$ 'delete$)
  (with-retry-serialization-failure ("delete")
    (with-transaction-type (repeatable-read-rw)
      (insert-old$ model id)
      (nth-value 0 (delete$ model id)))))

(defun keys (model)
  "Returns two values: a list of all primary keys for this MODEL, a
symbol, and the length of that list."
  (ensure-model-query model 'keys$)
  (with-transaction-type (read-committed-ro)
    (keys$ model)))
