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
;;;; This is the factory for make such queries

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

(defun-make-query count$ ()
    ('`(:select (:count '*) :from ,*table*)
     :single!))

;;;; Define a simple interace to disabiguate which query to use, based
;;;; on it's model.

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
(defun-query count$ ())

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
