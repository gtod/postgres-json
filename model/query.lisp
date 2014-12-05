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

(define-query ready-bookings$ (filter email-regex)
  (:order-by
   (:select (j->> "id" "name" "email")
    :from 'booking
    :where (:and (:or (:@> 'jdoc filter))
                 (:~ (j->> "email") email-regex)))
   (:type (j->> "price") real)))

;; Looks like for building json jocs, -> or ->> will do
(define-query animals$ ()
  (:select (j->> 'c.jdoc "name" "coat")
   :from (:as 'cat 'c)
   :inner-join (:as 'dog 'd)
   :on (:= (j->> 'c.jdoc "name") (j->> 'd.jdoc "name"))))

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

(defun walk-tree (fun tree)
  (subst-if t
            (constantly nil)
            tree
            :key fun))

(defun subst-params-in-query (query-params query-form)
  (let ((tree (copy-tree query-form)))
    (loop for param in query-params
          for i from 1
          do (nsubst `(quote ,(sym t "$" i)) param tree))
    tree))

;; (j->  "id")                        JSON from key "id" in 'jdoc
;; (j->> "id")                        text from key "id" in 'jdoc
;; (j->> "id" "name" "email")         a JSON obj with text values of those keys in 'jdoc
;; (j->  "id" "name" "email")         a JSON obj with JSON values of those keys in 'jdoc
;; (j->  'jdoc "id")                  JSON from key "id" in 'jdoc
;; (j->> 'jdoc "id")                  text from key "id" in 'jdoc
;; (j->> 'c.jdoc "id" "name" "email") a JSON obj with text values of those keys in 'jdoc
;; (j->  'd.jdoc "id" "name" "email") a JSON obj with JSON value sof those keys in 'jdoc
(defun nsubst-json-builder (form op tree)
  (labels ((nsubst-key (column key)
             (let ((new `(,op ,column ,key)))
               (nsubst new form tree :test #'equal)))
           (nsubst-keys (column keys)
             (let ((pairs '()))
               (dolist (key keys)
                 (push key pairs)
                 (push `(,op ,column ,key) pairs))
               (let ((new `(:json-build-object ,@(reverse pairs))))
                 (nsubst new form tree :test #'equal))))
           (explicit-column-p ()
             (listp (cadr form)))
           (single-key-p ()
             (= (if (explicit-column-p) 3 2) (length form))))
    (if (explicit-column-p)
        (if (single-key-p)
            (nsubst-key (cadr form) (caddr form))
            (nsubst-keys (cadr form) (cddr form)))
        (if (single-key-p)
            (nsubst-key '(quote jdoc) (cadr form))
            (nsubst-keys '(quote jdoc) (cdr form))))))

(defun subst-sugar-in-query (form)
  "Replace all POSTGRES-JSON syntactic sugar variants in the S-SQL
FORM with their true S-SQL representations."
  (let ((tree (copy-tree form)))
    (flet ((handle (form)
             (when (listp form)
               (cond ((eq 'j-> (car form)) (nsubst-json-builder form :-> tree))
                     ((eq 'j->> (car form)) (nsubst-json-builder form :->> tree))))))
      (walk-tree #'handle tree))
    tree))

(defmacro define-query (name (&rest query-params) &body query)
  "You must always return jdoc or use jbuild..."
  (let* ((form0 (subst-params-in-query query-params (car query)))
         (form (subst-sugar-in-query form0)))
    `(defprepared-with-args ,name ,form ,query-params :column)))
