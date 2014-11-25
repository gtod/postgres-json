(in-package :postgres-json)

;;;; Query management

(defparameter *query-functions* (make-hash-table :test #'equal))

(defun query-key (model operation)
  (format nil "~A:~A" (symbol-name model) (symbol-name operation)))

(defun lookup-query (model operation)
  (gethash (query-key model operation) *query-functions*))

(defun set-lookup-query (model operation query)
  (setf (gethash (query-key model operation) *query-functions*)
        query))

(defsetf lookup-query set-lookup-query)

;;;; Make queries paramaterized on model

(defun make-nextval-sequence$ (model sequence)
  (setf (lookup-query model 'nextval-sequence$)
        (prepare (sql-compile `(:select (:nextval ,sequence)))
            :single!)))

(defun make-insert$ (model table)
  (setf (lookup-query model 'insert$)
        (prepare (sql-compile `(:insert-into ,table :set ',*id* '$1 ',*jdoc* '$2
                                             :returning ',*id*))
            :single!)))

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
                                                      :where (:= ',*id* '$1)))))))

(defun make-update$ (model table)
  (setf (lookup-query model 'update$)
        (prepare (sql-compile `(:update ,table
                                :set ',*jdoc* '$2 'valid-from (:transaction-timestamp)
                                :where (:= ',*id* '$1)
                                :returning ',*id*))
            :single)))

(defun make-get$ (model table)
  (setf (lookup-query model 'get$)
        (prepare (sql-compile `(:select ',*jdoc* :from ,table :where (:= ',*id* '$1)))
            :single!)))

(defun make-delete$ (model table)
  (setf (lookup-query model 'delete$)
        (prepare (sql-compile `(:delete-from ,table :where (:= ',*id* '$1) :returning ',*id*))
            :single)))

(defun make-keys$ (model table)
  (setf (lookup-query model 'keys$)
        (prepare (sql-compile `(:select ',*id* :from ,table))
            :column)))

;;;; Make unparameterized queries

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

;;;; Model proper

;;; Need to comment on acceptable type of ID: integer, string, ??

(defun insert (model object &key use-id stash-id (to-json 'to-json))
  "Insert lisp object OBJECT into the backend MODEL, a symbol,
after JSON serialization.  If USE-ID is supplied, use that as the
primary key for this object rather than the automatically generated
one.  If STASH-ID-FN is a symbol we FUNCALL it with two arguments: the
value of the id to be used for the DB insert and OBJECT.  TO-JSON must
be a function designator for a function of one argument to serialize
lisp objects to JSON strings.  Return the id."
  (with-transaction-type (read-committed-rw)
    (let* ((id (if use-id use-id (nextval-sequence$ model)))
           (object (if stash-id
                       (funcall stash-id id object)
                       object)))
      (nth-value 0 (insert$ model id (funcall to-json object))))))

(defun update (model id object &key (to-json 'to-json))
  "Update the current value of the object with primary key ID in
backend MODEL, a symbol, to be the JSON serialization of OBJECT.
TO-JSON must be a function designator for a function of one argument
to serialize lisp objects to JSON strings.  Returns ID on success,
NIL if there was no such ID found."
  (log:debug "Attempt update of ~A" id)
  (with-retry-serialization-failure ("update")
    (with-transaction-type (repeatable-read-rw)
      (insert-old$ model id)
      (nth-value 0 (update$ model id (funcall to-json object))))))

(defun get (model id &key (from-json 'from-json))
  "Lookup the object with primary key ID in MODEL, a symbol, and
return a parse of the JSON string by the the function of one argument
designated by FROM-JSON.  Make it #'identity to return just the JSON
string proper."
  (funcall from-json (with-transaction-type (read-committed-ro)
                       (get$ model id))))

(defun delete (model id)
  "Delete the object with primary key ID.  Returns ID on success,
NIL if there was no such ID found."
  (log:debug "Attempt delete of ~A" id)
  (with-retry-serialization-failure ("delete")
    (with-transaction-type (repeatable-read-rw)
      (insert-old$ model id)
      (nth-value 0 (delete$ model id)))))

(defun keys (model)
  "Returns two values: a list of all primary keys for this MODEL, a
symbol, and the length of that list."
  (with-transaction-type (read-committed-ro)
    (keys$ model)))

;;;; Interface

(defun prepare-model (model &key (schema *db-schema*) (sequence *db-sequence*))
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
    (make-keys$ model table)
    model))
