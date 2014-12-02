(in-package :postgres-json)

;;; Cache our queries/model-parameters once we have created/found them.

;; Postmodern ensures that our prepared queries are in fact prepared
;; on demand for the connection that we request in our current thread.
;; So as long as we (and our clients) ensure the connection itself is
;; thread local there should be no contention when adding the on
;; demand prepared query to the connection's meta slot.  See pomo
;; postmodern/prepare.lisp.
(defparameter *query-functions* (make-hash-table :test #'equal)
  "Hash of (for example) \"cat:insert$\" => query function.")

(defparameter *model-parameters* (make-hash-table :test #'equal)
  "Hash of symbol model => model parameters.")

(defun query-key (model operation)
  (format nil "~A:~A:~A"
          (symbol-name *pgj-schema*)
          (symbol-name model)
          (symbol-name operation)))

(defun lookup-query (model operation)
  (gethash (query-key model operation) *query-functions*))

(defun set-lookup-query (model operation query)
  (setf (gethash (query-key model operation) *query-functions*)
        query))

(defsetf lookup-query set-lookup-query)

;;;; Our queries are made on demand for a schema/model/query-name
;;;; combination.  This is the factory for making such queries, and the
;;;; queries themselves.  The convention is to suffix queries proper
;;;; with #\$.

(defmacro make-query (name (&rest query-args) (&rest model-params)
                      (query &optional (format :rows)))
  "Defun both a function to _make_ a query called NAME and a function
for the query proper called NAME and accepting a list of symbols,
QUERY-ARGS, which become the numbered parameters of the query when it
is called.  MODEL-PARAMS must be a list of symbols, each a
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

;;; In model/interface.lisp we write (say): (get$ model id).  And if
;;; you macroexpand (make-query $get ...) below you will see this is
;;; correct.  But we omit the model arg from the QUERY-ARGS of the
;;; MAKE-QUERY macro because it does not end up being a parameter of
;;; the Postmodern prepared query...

(make-query nextval-sequence$ () (sequence)
    ('`(:select (:nextval ,(db-name-string sequence))) :single!))

(make-query insert$ (id jdoc) (table id jdoc)
    ('`(:insert-into ,table :set ',id '$1 ',jdoc '$2
                     :returning ',id)
     :single!))

(make-query insert-old$ (id) (table table-old id jdoc)
    ('`(:insert-into ,table-old
                   ;; Note the dependence on the column ordering of
                   ;; CREATE-OLD-TABLE since :insert-into will not let
                   ;; me explicitly specify column names...
                     (:select ',id
                            (:transaction-timestamp)
                            'valid-from
                            ',jdoc
                            :from ,table
                            :where (:= ',id '$1)))))

(make-query update$ (id jdoc) (table id jdoc)
    ('`(:update ,table
        :set ',jdoc '$2 'valid-from (:transaction-timestamp)
        :where (:= ',id '$1)
        :returning ',id)
     :single))

(make-query get$ (id) (table id jdoc)
    ('`(:select ',jdoc :from ,table :where (:= ',id '$1))
     :single))

(make-query all$ () (table jdoc)
    ('`(:select ',jdoc :from ,table)
     :column))

(make-query delete$ (id) (table id)
    ('`(:delete-from ,table :where (:= ',id '$1) :returning ',id)
     :single))

(make-query keys$ () (table id)
    ('`(:select ',id :from ,table)
     :column))

(make-query count$ () (table)
    ('`(:select (:count '*) :from ,table)
     :single!))

;;;; Functions in the model interface must ensure the DB queries they
;;;; intend to use exist, by calling ENSURE-MODEL-QUERY

(defun ensure-model-query (model &rest operations)
  (let ((model-parameters (if (eq *meta-model* model)
                              (meta-model-parameters)
                              (or (gethash model *model-parameters*)
                                  (setf (gethash model *model-parameters*)
                                        (get *meta-model* (symbol->json model)
                                             :from-json 'model-parameters-from-json))))))
    (dolist (op operations)
      (ensure-model-query-op model model-parameters op))
    model-parameters))

(defun ensure-model-query-op (model model-parameters operation)
  "If (say) cat:insert$ exists then return that query (a function).
If not we make the query OPERATION on demand after getting the
required parameters for MODEL from the meta model.  Of course, we fix
the bootstrap problem by calling a function to supply the meta model's
own parameters.  Returns the the model's parameters."
  (if (lookup-query model operation)
      (log:trace "Using prepared query for ~A:~A" model operation)
      (progn
        (log:trace "Preparing query for ~A:~A" model operation)
        (funcall (sym :postgres-json "make-" operation) model model-parameters))))
