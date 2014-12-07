(in-package :postgres-json)

;;;; Define the interface to our PostgreSQL JSON persistence model

;;; Need to comment on acceptable type of KEY: integer, string, ??

(defun insert (model object &key use-key (stash-key *stash-key*) (to-json *to-json*))
  "Insert lisp object OBJECT into the backend MODEL, a symbol,
after JSON serialization.  If USE-KEY is supplied, use that as the
primary key for this object rather than the automatically generated
one.  If STASH-KEY is non null we FUNCALL it with two arguments: the
value of the key to be used for the DB insert and OBJECT.  It should
return an object which will be inserted in the place of the original.
Typically you would use this to 'stash' the fresh primary key inside
your object.  TO-JSON must be a function designator for a function of
one argument to serialize lisp objects to JSON strings.  Return the
new primary key."
  (log:debu3 "Attempt insert of object into ~A" model)
  (unless use-key
    (ensure-model-query model 'nextval-sequence$))
  (ensure-model-query model 'insert$)
  (ensure-transaction-level (insert read-committed-rw)
    (let* ((key (if use-key use-key (nextval-sequence$ model)))
           (object (if stash-key (funcall stash-key key object) object)))
      (nth-value 0 (insert$ model key (funcall to-json object))))))

(defun update (model key object &key (stash-key *stash-key*) (to-json *to-json*))
  "Update the current value of the object with primary key KEY, of
type compatible with Postgres type KEY-TYPE in the model's parameters,
in backend MODEL, a symbol, to be the JSON serialization of OBJECT.
If STASH-KEY is non null we FUNCALL it with two arguments: the value
of the key to be used for the DB insert and OBJECT.  It should return
an object which will be used in the place of the original.  TO-JSON
must be a function designator for a function of one argument to
serialize lisp objects to JSON strings.  Returns KEY on success, NIL
if there was no such KEY found."
  (log:debu3 "Attempt update of ~A in ~A" key model)
  (ensure-model-query model 'insert-old$ 'update$)
  (ensure-transaction-level (update repeatable-read-rw)
    (insert-old$ model key)
    (let ((object (if stash-key (funcall stash-key key object) object)))
      (nth-value 0 (update$ model key (funcall to-json object))))))

(defun get (model key &key (from-json *from-json*))
  "Lookup the object with primary key KEY (of type compatible with
Postgres type KEY-TYPE in the model's parameters, in MODEL, a symbol.
If such an object exists return a parse of the JSON string by the the
function of one argument designated by FROM-JSON (make it #'identity
to return just the JSON string proper).  If the object does not exist,
return nil."
  (log:debu4 "Get object with key ~A from ~A" key model)
  (ensure-model-query model 'get$)
  (let ((jdoc (ensure-transaction-level (get read-committed-ro)
                (get$ model key))))
    (if jdoc (funcall from-json jdoc) nil)))

(defun get-all (model &key (from-json *from-json*))
  "Return a list of all objects in MODEL, a symbol.
Each JSON string is parse by the the function of one argument
designated by FROM-JSON."
  (log:debu4 "Get all objects from ~A" model)
  (ensure-model-query model 'get-all$)
  (ensure-transaction-level (get read-committed-ro)
    (mapcar from-json (get-all$ model))))

(defun delete (model key)
  "Delete the object with primary key KEY, of type compatible with
Postgres type KEY-TYPE in the model's parameters.  Returns KEY on
success, NIL if there was no such KEY found."
  (log:debu3 "Attempt delete of object with key ~A from ~A" key model)
  (ensure-model-query model 'insert-old$ 'delete$)
  (ensure-transaction-level (delete repeatable-read-rw)
    (insert-old$ model key)
    (nth-value 0 (delete$ model key))))

(defun delete-all (model)
  "Delete all objects in MODEL, a symbol.  In fact this is a
recoverable operation in a sense as all deleted rows will still be in
the <model>-old Postgres relation."
  (log:debu3 "Attempt delete all from ~A" model)
  (with-model-transaction ()
    (dolist (key (keys model))
      (delete model key))))

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

;; Implicit (or even explicit) assumption here that you are storing
;; objects in your model, rather than arrays...
(defun filter (model &key contain keys limit
                          (to-json *to-json*) (from-json *from-json*))
  "Return all objects in MODEL, a symbol, which 'contain' in the
Postgres @> operator sense the object CONTAIN, which must serialize to
a JSON object.  If CONTAIN is nil, apply no containment restriction.
KEYS may be a list of strings being top level keys in the objects of
model and only the values of said keys will be returned, bundled
together in a JSON object.  If keys is nil the entire object will be
returned.  LIMIT, if supplied, must be an integer that represents the
maximum number of objects that will be returned.  FILTER will be JSON
serialized by TO-JSON, a function designator for a function of one
argument.  The returned JSON strings are parsed by the function of one
argument designated by FROM-JSON.  This is _not_ a prepared query so
extra care must be taken if KEYS or CONTAIN derive from unsanitized
user input."
  (let ((filter (if contain (funcall to-json contain) nil)))
    (let ((select `(:select ,(if keys `(jbuild ,keys) 'jdoc)
                    :from ',model
                    :where ,(if filter `(:@> 'jdoc ,filter) "t"))))
      (let ((query (if (integerp limit) `(:limit ,select ,limit) select)))
        (ensure-transaction-level (filter read-committed-ro)
          (mapcar from-json
                  (query (sql-compile (json-query-to-s-sql query))
                         :column)))))))

(defun exists (model key &key (from-json *from-json*))
  "Return all objects in MODEL, a symbol, which have a top level
object key or array element KEY, a string, in the Postgres ? sense.
The returned JSON object strings are parsed by the function of one
argument designated by FROM-JSON.  Requires a Potgres GIN index
without JSONB_PATH_OPS on MODEL."
  (log:trace "Call exists on ~A" model)
  (ensure-model-query model 'exists$)
  (ensure-transaction-level (contains read-committed-ro)
    (mapcar from-json (exists$ model key))))

(defun history (model key &key (from-json *from-json*))
  "Returns a list, in chronological order, of all previous values of
the object with primary key KEY, of type compatible with Postgres type
KEY-TYPE in the model's parameters, in MODEL, a symbol.  If such
objects exist return a parse of each JSON string by the the function
of one argument designated by FROM-JSON.  If the object has no
history, return nil."
  (log:debu4 "List history of object with key ~A from ~A" key model)
  (ensure-model-query model 'history$)
  (let ((column (ensure-transaction-level (get read-committed-ro)
                  (history$ model key))))
    (mapcar from-json column)))
