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

(make-query filter$ (json) (jdoc table)
    ('`( :select ',jdoc
         :from ,table
         :where (:@> ',jdoc '$1))
     :column))

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

;;;; New and improved JSON queries

(defun subst-params-into-query-form (query-params query-form)
  (let ((tree (copy-tree query-form)))
    (loop for param in query-params
          for i from 1
          do (nsubst `(quote ,(sym t "$" i)) param tree))
    (print tree)
    tree))

(defmacro define-query (name (&rest query-params) &body query)
  "You must always return jdoc or use jbuild..."
  (let* ((form (subst-params-into-query-form query-params (car query))))
    `(defprepared-with-args ,name ,form ,query-params :column)))

(define-query ready-bookings$ (filter email-regex)
  (:order-by
   (:select (:json-build-object "id" (:->> 'jdoc "id")
                                "name" (:->> 'jdoc "name")
                                "email" (:->> 'jdoc "email"))
    :from 'booking
    :where (:and (:or (:@> 'jdoc filter))
                 (:~ (:->> 'jdoc "email") email-regex)))
   (:type (:->> 'jdoc "price") real)))

;; Looks like for building json jocs, -> or ->> will do
(define-query animals$ ()
  (:select (:json-build-object "name" (:-> 'c.jdoc "name")
                               "coat" (:-> 'c.jdoc "coat"))
   :from (:as 'cat 'c)
   :inner-join (:as 'dog 'd)
   :on (:= (:->> 'c.jdoc "name") (:->> 'd.jdoc "name"))))

;; We can use the -> operator to return JSON, no need to build it,
;; we can just parse it when we get it!
;; We need the ->> form for sorting by int (say)
(define-query cat$ ()
  (:select (:-> 'jdoc "owns")
   :from 'cat
   :where (:= "fred" (:->> 'jdoc "name"))))

(defun ready-bookings (model filter-object email-regex
                       &key (to-json *to-json*) (from-json *from-json*))
  (ensure-model-query model 'ready-bookings$)
  (ensure-transaction-level (filter read-committed-ro)
    (mapcar from-json (ready-bookings$ (funcall to-json filter-object) email-regex))))


;; Helpers should provide some syntactic sugar
;; and do the automatic to-json for incoming filters
;; and the from-json from returing JSON
;; Maybe we specify if it *returns* json (the default)
;; And maybe we specific incoming params that need to-json


;;; You know, key and jdoc as mode params are just a wast of time.
;;; They should be hard coded, what hope have we got of writing queries?
;;; But what of compound primary keys?
;;; Do we need model params at all?  table and old table can be made on demand
;;; from the model symbol!!
;;; Well, we do need key-type (eg. uuid) and (maybe) jdoc-type.  But would
;;; be simpler if it was just key (compound) and key-type...
