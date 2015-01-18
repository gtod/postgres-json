(in-package :postgres-json)

;;;; Add keeping a permanent history of all rows to basic PGJ-MODEL

(defgeneric model-old-table (model)
  (:documentation "The Postgres qualified name as an S-SQL form for a
history or 'old' table of MODEL.")
  (:method ((model pgj-history-model))
    (qualified-name (sym-suffix (model-name model) "old"))))

(defgeneric create-old-table (model)
  (:documentation "Create a Postgres table to contain the previous
values of JSON documents in the base table of MODEL.")
  (:method ((model pgj-history-model))
    (let ((old-table (sql-compile (model-old-table model)))
          (key-name (model-key-name model))
          (key-type (model-key-type model)))
      (run `(:create-table ,old-table
             ((,key-name  :type ,key-type )
              (valid-to   :type timestamptz)
              (valid-from :type timestamptz)
              (jdoc       :type jsonb))
             (:primary-key ,key-name valid-to))))))

(defmethod create-backend ((model pgj-history-model))
  (call-next-method)
  (create-old-table model))

(defmethod backend-exists-p ((model pgj-history-model))
  (and (call-next-method)
       (%table-exists-p (sql-compile (model-old-table model)))))

(defmethod drop-backend ((model pgj-history-model))
  (call-next-method)
  (drop-db-table-cascade (sql-compile (model-old-table model))))
