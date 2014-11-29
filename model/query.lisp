(in-package :postgres-json)

;;;; Cache our queries once we have created them

(defparameter *query-functions* (make-hash-table :test #'equal)
  "Hash of (for example) \"cat:insert$\" => query function.")

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

;;;; Our queries are made on demand for a model/query-name combination
;;;; This is the factory for make such queries, and the queries
;;;; themselves.  The convention is to suffix function that actually
;;;; go to the backend DB with #\$.

(defmacro make-query (name (&rest args) (query &optional (format :rows)))
  `(progn
     (defun ,(sym t "make-" name) (model)
       (setf (lookup-query model ',name)
             (prepare (sql-compile ,@(cdr query)) ,format)))
     (defun ,name (model ,@args)
       (funcall (lookup-query model ',name) ,@args))))

(make-query nextval-sequence$ ()
    ('`(:select (:nextval ,(qualified-name-string *sequence* *pgj-schema*))) :single!))

(make-query insert$ (id jdoc)
    ('`(:insert-into ,*table* :set ',*id* '$1 ',*jdoc* '$2
                     :returning ',*id*)
     :single!))

(make-query insert-old$ (id)
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

(make-query update$ (id jdoc)
    ('`(:update ,*table*
        :set ',*jdoc* '$2 'valid-from (:transaction-timestamp)
        :where (:= ',*id* '$1)
        :returning ',*id*)
     :single))

(make-query get$ (id)
    ('`(:select ',*jdoc* :from ,*table* :where (:= ',*id* '$1))
     :single!))

(make-query delete$ (id)
    ('`(:delete-from ,*table* :where (:= ',*id* '$1) :returning ',*id*)
     :single))

(make-query keys$ ()
    ('`(:select ',*id* :from ,*table*)
     :column))

(make-query count$ ()
    ('`(:select (:count '*) :from ,*table*)
     :single!))

;;;; Functions in the model interface must ensure the DB queries they
;;;; intend to use exist, by calling ENSURE-MODEL-QUERY

(defun ensure-model-query (model &rest operations)
  (dolist (op operations)
    (ensure-model-query-op model op)))

(defun ensure-model-query-op (model operation)
  "If (say) cat:insert$ exists then return that query (a function).
If not we make the query OPERATION on demand after getting the
required parameters for MODEL from the meta model.  Of course, we fix
the bootstrap problem by calling a function to supply the meta model's
own parameters."
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
