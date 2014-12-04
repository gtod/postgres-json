(in-package :postgres-json)

;;;; Cache our queries/model-parameters once we have created/found them.

;; If we use Postmodern's PREPARE (and kin) to prepare a Postgres
;; query, Postmodern ensures that the query is prepared on _every_
;; connection automatically.  So as long as we use a distinct Postgres
;; connection for every thread we run (it would be hard to do
;; otherwise) everything should just work...  See pomo
;; postmodern/prepare.lisp.
(defparameter *query-functions* (make-hash-table :test #'equal)
  "Hash of (for example) \"cat:insert$\" => query function.")

(defun query-key (model operation)
  "Return a string: *PGJ-SCHEMA*:MODEL:OPERATION to key a prepared
query.  An 'operation' is the name of a DB query, for example GET$."
  (format nil "~A:~A:~A"
          (symbol-name *pgj-schema*)
          (symbol-name model)
          (symbol-name operation)))

(defun lookup-query (model operation)
  "Return the prepared query for the triple *PGJ-SCHEMA*,
MODEL, OPERATION."
  (gethash (query-key model operation) *query-functions*))

(defun set-lookup-query (model operation query)
  "Set the prepared query for the triple *PGJ-SCHEMA*,
MODEL, OPERATION."
  (setf (gethash (query-key model operation) *query-functions*)
        query))

(defsetf lookup-query set-lookup-query)

;;;; Our queries are made on demand for a schema/model/query-name
;;;; combination.  This is the factory for creating such 'make-'
;;;; queries, and also the lookup function for each query.  The
;;;; convention is to suffix queries proper (aka 'operations') with
;;;; #\$.

(defmacro make-query (name (&rest query-args) (&rest model-params)
                      (query &optional (format :rows)))
  "Defun both a function to _make_ a query called NAME and a function
to lookup and execute the query called NAME accepting a list of
symbols, QUERY-ARGS, which become the numbered parameters of the query
when it is called.  MODEL-PARAMS must be a list of symbols, each a
model-parameter.  These are bound in the make query function to the
value of said model-parameter, an object that must be passed as the
second argument to the make function (the first argument being the
model).  QUERY is a qoted backquoted S-SQL query form optionally
containing evaluated expressions including the bound model-parameters.
FORMAT must be a valid Postmodern results format."
  `(progn
     (defun ,(sym t "make-" name) (model model-parameters)
       (declare (ignorable model-parameters))
       (with-readers (,@(loop for param in model-params collect param)) model-parameters
         (setf (lookup-query model ',name)
               (prepare (sql-compile ,@(cdr query)) ,format))))
     (defun ,name (model ,@query-args)
       (funcall (lookup-query model ',name) ,@query-args))))

;;; In model/interface.lisp we write (say): (get$ model key).  And if
;;; you macroexpand (make-query $get ...) below you will see this is
;;; correct.  But we omit the model arg from the QUERY-ARGS of the
;;; MAKE-QUERY macro because it does not end up being a parameter of
;;; the Postmodern prepared query...

(make-query nextval-sequence$ () (sequence)
    ('`(:select (:nextval ,(db-name-string sequence))) :single!))

(make-query insert$ (key jdoc) (table key jdoc)
    ('`(:insert-into ,table :set ',key '$1 ',jdoc '$2
                     :returning ',key)
     :single!))

(make-query insert-old$ (key) (table table-old key jdoc)
    ('`(:insert-into ,table-old
                   ;; Note the dependence on the column ordering of
                   ;; CREATE-OLD-TABLE since :insert-into will not let
                   ;; me explicitly specify column names...
                     (:select ',key
                            (:transaction-timestamp)
                            'valid-from
                            ',jdoc
                            :from ,table
                            :where (:= ',key '$1)))))

(make-query update$ (key jdoc) (table key jdoc)
    ('`(:update ,table
        :set ',jdoc '$2 'valid-from (:transaction-timestamp)
        :where (:= ',key '$1)
        :returning ',key)
     :single))

(make-query get$ (key) (table key jdoc)
    ('`(:select ',jdoc :from ,table :where (:= ',key '$1))
     :single))

(make-query all$ () (table jdoc)
    ('`(:select ',jdoc :from ,table)
     :column))

(make-query delete$ (key) (table key)
    ('`(:delete-from ,table :where (:= ',key '$1) :returning ',key)
     :single))

(make-query keys$ () (table key)
    ('`(:select ',key :from ,table)
     :column))

(make-query count$ () (table)
    ('`(:select (:count '*) :from ,table)
     :single!))

;;;; Functions in the model interface must ensure the DB queries they
;;;; intend to use exist, by calling ENSURE-MODEL-QUERY

(defun ensure-model-query (model &rest operations)
  "Call ENSURE-MODEL-QUERY-OP on each element of the list
(of symbols) OPERATIONS, using MODEL, a symbol."
  (dolist (op operations)
    (ensure-model-query-op model op)))

(defun ensure-model-query-op (model operation)
  "If (say) my-schema:cat:insert$ exists then return that query (a
function).  If not make the query OPERATION (a symbol matching one of
the MAKE-QUERY forms defined above) on demand after getting the
required parameters for MODEL from the meta model.  Of course, we fix
the bootstrap problem by calling a function to supply the meta model's
own parameters."
  (if (lookup-query model operation)
      (log:trace "Using prepared query for ~A:~A" model operation)
      (let ((model-parameters (if (meta-model-p model)
                                  (meta-model-parameters)
                                  (get-model-parameters model))))
        (log:trace "Preparing query for ~A:~A" model operation)
        (funcall (sym :postgres-json "make-" operation) model model-parameters))))
