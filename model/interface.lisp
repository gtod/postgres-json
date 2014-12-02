(in-package :postgres-json)

;;;; Define the interface to our PostgreSQL JSON persistence model

;;; Need to comment on acceptable type of ID: integer, string, ??

(defun insert (model object &key use-id stash-id (to-json *to-json*))
  "Insert lisp object OBJECT into the backend MODEL, a symbol,
after JSON serialization.  If USE-ID is supplied, use that as the
primary key for this object rather than the automatically generated
one.  If STASH-ID is a symbol we FUNCALL it with two arguments: the
value of the id to be used for the DB insert and OBJECT.  It should
return another object which will be inserted in the place of the
original.  Typically you would use this to 'stash' the fresh primary
key inside your object.  TO-JSON must be a function designator for a
function of one argument to serialize lisp objects to JSON strings.
Return the id."
  (log:debu3 "Attempt insert of object into ~A" model)
  (unless use-id
    (ensure-model-query model 'nextval-sequence$))
  (ensure-model-query model 'insert$)
  (ensure-transaction-level (insert read-committed-rw)
    (let* ((id (if use-id use-id (nextval-sequence$ model)))
           (object (if stash-id
                       (funcall stash-id id object)
                       object)))
      (nth-value 0 (insert$ model id (funcall to-json object))))))

(defun update (model id object &key (to-json *to-json*))
  "Update the current value of the object with primary key ID (of type
compatible with Postgres type *ID-TYPE*) in backend MODEL, a symbol,
to be the JSON serialization of OBJECT.  TO-JSON must be a function
designator for a function of one argument to serialize lisp objects to
JSON strings.  Returns ID on success, NIL if there was no such ID
found."
  (log:debu3 "Attempt update of ~A in ~A" id model)
  (ensure-model-query model 'insert-old$ 'update$)
  (ensure-transaction-level (update repeatable-read-rw)
    (insert-old$ model id)
    (nth-value 0 (update$ model id (funcall to-json object)))))

(defun get (model id &key (from-json *from-json*))
  "Lookup the object with primary key ID (of type compatible with
Postgres type *ID-TYPE*) in MODEL, a symbol, and return a parse of the
JSON string by the the function of one argument designated by
FROM-JSON.  Make it #'identity to return just the JSON string proper."
  (log:debu4 "Get object with key ~A from ~A" id model)
  (ensure-model-query model 'get$)
  (funcall from-json (ensure-transaction-level (get read-committed-ro)
                       (get$ model id))))

(defun delete (model id)
  "Delete the object with primary key ID (of type compatible with
Postgres type *ID-TYPE*).  Returns ID on success, NIL if there was no
such ID found."
  (log:debu3 "Attempt delete of object with key ~A from ~A" id model)
  (ensure-model-query model 'insert-old$ 'delete$)
  (ensure-transaction-level (delete repeatable-read-rw)
    (insert-old$ model id)
    (nth-value 0 (delete$ model id))))

(defun keys (model)
  "Returns two values: a list of all primary keys for this MODEL, a
symbol, and the length of that list."
  (log:trace "Call keys on ~A" model)
  (ensure-model-query model 'keys$)
  (ensure-transaction-level (keys read-committed-ro)
    (keys$ model)))

(defun count (model)
  "Returns the number of entries in MODEL, a symbol."
  (log:trace "Call count on ~A" model)
  (ensure-model-query model 'count$)
  (ensure-transaction-level (count read-committed-ro)
    (nth-value 0 (count$ model))))
