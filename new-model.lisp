(in-package :postgres-json)

;; (defclass postgres-json-persist-model () ())

;; (defclass cat (postgres-json-persist-model) ())

;; (defgeneric insert$ (id json)
;;   (:documentation "Insert JSON into a Postgres relation using primary
;; key ID."))

;; (defmethod insert$ (()))

(defparameter *query-functions* (make-hash-table :test #'equal))

(defun query-key (label operation)
  (format nil "~A:~A" (symbol-name label) (symbol-name operation)))

(defun lookup-query (label operation)
  (gethash (query-key label operation) *query-functions*))

(defun set-lookup-query (label operation query)
  (setf (gethash (query-key label operation) *query-functions*)
        query))

(defsetf lookup-query set-lookup-query)

(defun insert (label object &key use-id (to-json 'to-json) stash-id)
  (with-transaction-type (read-committed-rw)
    (let* ((id (if use-id use-id (nextval-sequence$ label)))
           (object (if stash-id
                       (funcall stash-id id object)
                       object)))
      (insert$ label id (funcall to-json object))
      id)))

(defun insert$ (label id json)
  (funcall (lookup-query label 'insert$) id json))

(defun make-insert$ (label table)
  (setf (lookup-query label 'insert$)
        (prepare (sql-compile `(:insert-into ,table :set ',*id* '$1 ',*jdoc* '$2)))))

(defun nextval-sequence$ (label)
  (funcall (lookup-query label 'nextval-sequence$)))

(defun make-nextval-sequence$ (label sequence)
  (setf (lookup-query 'nextval-sequence$)
        (prepare (sql-compile `(:select (:nextval ,sequence)))
            :single!)))

(defun prepare-model (label &key (schema *db-schema*) (sequence *db-sequence*))
  (with-connection '("cusoon" "gtod" "" "localhost" :port 5433)
    (make-insert$ label (qualified-name label schema))
    (make-nextval-sequence$ label (qualified-name-string sequence schema))))
