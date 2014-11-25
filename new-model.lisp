(in-package :postgres-json)

;; (defclass postgres-json-persist-model () ())

;; (defclass cat (postgres-json-persist-model) ())

;; (defgeneric insert$ (id json)
;;   (:documentation "Insert JSON into a Postgres relation using primary
;; key ID."))

;; (defmethod insert$ (()))

(defparameter *query-functions* (make-hash-table :test #'equal))

(defun query-key (model operation)
  (format nil "~A:~A" (symbol-name model) (symbol-name operation)))

(defun lookup-query (model operation)
  (gethash (query-key model operation) *query-functions*))

(defun set-lookup-query (model operation query)
  (setf (gethash (query-key model operation) *query-functions*)
        query))

(defsetf lookup-query set-lookup-query)

(defun insert (model object &key use-id (to-json 'to-json) stash-id)
  (with-transaction-type (read-committed-rw)
    (let* ((id (if use-id use-id (nextval-sequence$ model)))
           (object (if stash-id
                       (funcall stash-id id object)
                       object)))
      (insert$ model id (funcall to-json object))
      id)))

(defun insert$ (model id json)
  (funcall (lookup-query model 'insert$) id json))

(defun nextval-sequence$ (model)
  (funcall (lookup-query model 'nextval-sequence$)))

;;;; Make queries paramaterized on model

(defun make-nextval-sequence$ (model sequence)
  (setf (lookup-query model 'nextval-sequence$)
        (prepare (sql-compile `(:select (:nextval ,sequence)))
            :single!)))

(defun make-insert$ (model table)
  (setf (lookup-query model 'insert$)
        (prepare (sql-compile `(:insert-into ,table :set ',*id* '$1 ',*jdoc* '$2)))))

(defun make-insert-old$ (model table old-table)
  (setf (lookup-query model 'insert-old$)
        (prepare (sql-compile `(:insert-into ,old-table
                                             ;; Note the dependence on the column ordering of
                                             ;; CREATE-OLD-TABLE since :insert-into will not let
                                             ;; me explicitly specify column names...
                                             (:select ',*id*
                                                      (:transaction-timestamp)
                                                      'valid-from
                                                      ',*jdoc*
                                                      :from ,table
                                                      :where (:= ',*id* '$1))
                                             :returning ',*id*)))))

(defun make-update$ (model table)
  (setf (lookup-query model 'update$)
        (prepare (sql-compile `(:update ,table
                                :set ',*jdoc* '$2 'valid-from (:transaction-timestamp)
                                :where (:= ',*id* '$1)
                                :returning ',*id*))
            :single!)))

(defun make-get$ (model table)
  (setf (lookup-query model 'get$)
        (prepare (sql-compile `(:select ',*jdoc* :from ,table :where (:= ',*id* '$1)))
            :single!)))

(defun make-delete$ (model table)
  (setf (lookup-query model table)
        (prepare (sql-compile `(:delete-from ,table :where (:= ',*id* '$1) :returning ',*id*))
            :single)))

(defun make-get-all-ids$ (model table)
  (setf (lookup-query model table)
        (prepare (sql-compile `(:select ',*id* :from ,table))
            :column)))

(defun prepare-model (model &key (schema *db-schema*) (sequence *db-sequence*))
  (with-connection '("cusoon" "gtod" "" "localhost" :port 5433)
    (let* ((base model)
           (old (sym t base "-old"))
           (table (qualified-name base schema))
           (table-old (qualified-name old schema)))
      (make-nextval-sequence$ model (qualified-name-string sequence schema))
      (make-insert$ model table)
      (make-insert-old$ model table table-old)
      (make-update$ model table)
      (make-get$ model table)
      (make-delete$ model table)
      (make-get-all-ids$ model table))))
