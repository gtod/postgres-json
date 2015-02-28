(in-package :postgres-json)

;;;; Postmodern queries proper and support

(defun query-key (model-name operation)
  "Return a string: *PGJ-SCHEMA*:MODEL-NAME:OPERATION to key a prepared
query.  An 'operation' is the name of a DB query, for example FETCH$."
  (format nil "~A:~A:~A"
          (symbol-name *pgj-schema*)
          model-name
          (symbol-name operation)))

(defun lookup-query (model-name operation)
  "Return the prepared query for the triple *PGJ-SCHEMA*,
MODEL, OPERATION."
  (gethash (query-key model-name operation) *query-functions*))

(defun set-lookup-query (model-name operation query)
  "Set the prepared query for the triple *PGJ-SCHEMA*,
MODEL, OPERATION."
  (setf (gethash (query-key model-name operation) *query-functions*)
        query))

(defsetf lookup-query set-lookup-query)

;;;; Our queries are made on demand for a schema/model/query-name
;;;; combination.  This is the factory for creating such queries.  The
;;;; convention is to suffix queries proper (aka 'operations') with
;;;; #\$.

(defmacro make-query (name (&rest query-args) (&rest model-params)
                      (query-form &optional (format :rows)))
  "Defun a function called NAME, a symbol, accepting an instance of
PGJ-MODEL as the first argument followed by zero or more symbols,
QUERY-ARGS, which become the numbered parameters supplied to a
Postmodern query when NAME is called.  MODEL-PARAMS may be a list of
symbols which will be bound when making the query to the result of
calling model-<param> on MODEL, the first argument to the function.
QUERY-FORM is a quoted backquoted S-SQL query form optionally containing
evaluated expressions including the bound values.  FORMAT must be a
valid Postmodern results format."
  `(defun ,name (model ,@query-args)
     (let ((model-name (model-name model)))
       (symbol-macrolet ((query (lookup-query model-name ',name)))
         (unless query
           (let ,(loop for param in model-params
                       collect (list param `(,(sym-suffix 'model param) model)))
             (setf query (prepare (sql-compile ,@(cdr query-form)) ,format))))
         (funcall query ,@query-args)))))

;;;; Queries proper to implement model interface functions

;;; Base queries

(make-query nextval-sequence$ () (sequence)
    ('`(:select (:nextval ,(qualified-name-string sequence))) :single!))

(make-query insert$ (key jdoc) (table key-name)
    ('`(:insert-into ,table :set ',key-name '$1 'jdoc '$2
                     :returning ',key-name)
     :single!))

(make-query supersede$ (key jdoc) (table key-name)
    ('`(:update ,table
        :set 'jdoc '$2 'valid-from (:clock-timestamp)
        :where (:= ',key-name '$1)
        :returning ',key-name)
     :single))

(make-query fetch$ (key) (table key-name)
    ('`(:select 'jdoc :from ,table :where (:= ',key-name '$1))
     :single))

(make-query fetch-all$ () (table)
    ('`(:select 'jdoc :from ,table)
     :column))

(make-query excise$ (key) (table key-name)
    ('`(:delete-from ,table :where (:= ',key-name '$1) :returning ',key-name)
     :single))

(make-query excise-all$ () (table)
    ('`(:delete-from ,table)
     :single))

(make-query keys$ () (table key-name)
    ('`(:select ',key-name :from ,table)
     :column))

(make-query arb-key$ () (table key-name)
    ('`(:limit (:select ',key-name :from ,table) 1)
     :single))

(make-query tally$ () (table)
    ('`(:select (:count '*) :from ,table)
     :single!))

(make-query exists$ (json) (table)
    ('`( :select 'jdoc
         :from ,table
         :where (:? 'jdoc '$1))
     :column))

;;; History queries

(make-query insert-old$ (key) (table old-table key-name)
    ('`(:insert-into ,old-table
                   ;; Note the dependence on the column ordering of
                   ;; CREATE-OLD-TABLE since :insert-into will not let
                   ;; me explicitly specify column names...
                     (:select ',key-name
                            (:clock-timestamp)
                            'valid-from
                            'jdoc
                            :from ,table
                            :where (:= ',key-name '$1)))))

(make-query history$ (key) (old-table key-name)
    ('`(:order-by
        (:select 'jdoc 'valid-from 'valid-to
         :from ,old-table
         :where (:= ',key-name '$1))
        'valid-to)
     :rows))
