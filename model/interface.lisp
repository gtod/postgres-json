(in-package :postgres-json)

;;;; Define the interface to our PostgreSQL JSON persistence model

;;; Need to comment on acceptable type of KEY: integer, string, ??

(defun insert (model object &key use-key (stash-key *stash-key*) (to-json *to-json*))
  "Insert lisp object OBJECT into the backend MODEL, a symbol,
after JSON serialization.  If USE-KEY is supplied, use that as the
primary key for the JSON document rather than the automatically
generated one.  If STASH-KEY is non null we FUNCALL it with two
arguments: the value of the key to be used for the DB insert and
OBJECT.  It should return an object which will be inserted in the
place of the original.  Typically you would use this to 'stash' the
fresh primary key inside your object before serialization.  TO-JSON
must be a function designator for a function of one argument to
serialize lisp objects to JSON strings.  Return the new primary key."
  (log:debu3 "Attempt insert of object into ~A" model)
  (unless use-key
    (ensure-model-query model 'nextval-sequence$))
  (ensure-model-query model 'insert$)
  (ensure-transaction-level (insert read-committed-rw)
    (let* ((key (if use-key use-key (nextval-sequence$ model)))
           (object (if stash-key (funcall stash-key key object) object)))
      (nth-value 0 (insert$ model key (funcall to-json object))))))

(defun update (model key object &key (stash-key *stash-key*) (to-json *to-json*))
  "Update the current value of the JSON document with primary key KEY,
in backend MODEL, a symbol, to be the JSON serialization of lisp
object OBJECT.  If STASH-KEY is non null we FUNCALL it with two
arguments: the value of the key to be used for the DB insert and
OBJECT.  It should return an object which will be used in the place of
the original.  TO-JSON must be a function designator for a function of
one argument to serialize lisp objects to JSON strings.  Return KEY
on success, NIL if there was no such KEY found."
  (log:debu3 "Attempt update of ~A in ~A" key model)
  (ensure-model-query model 'insert-old$ 'update$)
  (ensure-transaction-level (update repeatable-read-rw)
    (insert-old$ model key)
    (let ((object (if stash-key (funcall stash-key key object) object)))
      (nth-value 0 (update$ model key (funcall to-json object))))))

(defun fetch (model key &key (from-json *from-json*))
  "Lookup the JSON document with primary key KEY in MODEL, a symbol.
If such a document exists return a parse of it by the function of one
argument designated by FROM-JSON (make it #'IDENTITY to return just
the JSON string proper).  If the JSON document does not exist, return
NIL."
  (log:debu4 "Fetch object with key ~A from ~A" key model)
  (ensure-model-query model 'fetch$)
  (let ((jdoc (ensure-transaction-level (fetch read-committed-ro)
                (fetch$ model key))))
    (if jdoc (funcall from-json jdoc) nil)))

(defun fetch-all (model &key (from-json *from-json*))
  "Return a list of all JSON documents in MODEL, a symbol, after
parsing by the the function of one argument designated by FROM-JSON."
  (log:debu4 "Fetch all objects from ~A" model)
  (ensure-model-query model 'fetch-all$)
  (ensure-transaction-level (fetch-all read-committed-ro)
    (mapcar from-json (fetch-all$ model))))

(defun excise (model key)
  "Delete the JSON document with primary key KEY from MODEL, a symbol.
Return KEY on success, NIL if there was no such KEY found."
  (log:debu3 "Call excise of object with key ~A from ~A" key model)
  (ensure-model-query model 'insert-old$ 'excise$)
  (ensure-transaction-level (excise repeatable-read-rw)
    (insert-old$ model key)
    (nth-value 0 (excise$ model key))))

(defun excise-all (model)
  "Delete all JSON documents in MODEL, a symbol.  In fact this is a
recoverable operation in a sense as all deleted rows will still be in
the <model>-old Postgres relation."
  (log:debu3 "Call excise-all from ~A" model)
  (with-model-transaction ()
    (dolist (key (keys model))
      (excise model key))))

(defun keys (model)
  "Returns two values: a list of all primary keys for this MODEL, a
symbol, and the length of that list."
  (log:trace "Call keys on ~A" model)
  (ensure-model-query model 'keys$)
  (ensure-transaction-level (keys read-committed-ro)
    (keys$ model)))

(defun tally (model)
  "Return a count of JSON documents in MODEL, a symbol."
  (log:trace "Call tally on ~A" model)
  (ensure-model-query model 'tally$)
  (ensure-transaction-level (count read-committed-ro)
    (nth-value 0 (tally$ model))))

;; Implicit (or even explicit) assumption here that you are storing
;; objects in your model, rather than arrays...
(defun filter (model &key contain properties limit
                          (to-json *to-json*) (from-json *from-json*))
  "Filter all JSON documents in MODEL, a symbol as follows.  Each
document must 'contain', in the Postgres @> operator sense, the object
CONTAIN which itself must serialize to a JSON document.  If CONTAIN is
NIL, apply no containment restriction.  PROPERTIES may be a list of
strings being top properties in the top level objects of the JSON
documents in MODEL and only the values of said properties will be
returned, bundled together in a JSON document.  If PROPERTIES is NIL
the entire JSON document will be returned.  LIMIT, if supplied, must
be an integer that represents the maximum number of objects that will
be returned.  CONTAIN will be JSON serialized by TO-JSON, a function
designator for a function of one argument.  The returned JSON
documents will be parsed by the function of one argument designated by
FROM-JSON.  Note that this is _not_ a prepared query so extra care
must be taken if PROPERTIES or CONTAIN derive from unsanitized user
input."
  (let ((filter (if contain (funcall to-json contain) nil)))
    (let ((select `(:select ,(if properties `(jbuild ,properties) 'jdoc)
                    :from ',model
                    :where ,(if filter `(:@> 'jdoc ,filter) "t"))))
      (let ((query (if (integerp limit) `(:limit ,select ,limit) select)))
        (ensure-transaction-level (filter read-committed-ro)
          (mapcar from-json
                  (query (sql-compile (json-query-to-s-sql query))
                         :column)))))))

(defun exists (model property &key (from-json *from-json*))
  "Return all JSON documents in MODEL, a symbol, which have a top
level object property PROPERTY, a string, or if said string appears as
an element of a top level array.  This is in the Postgres operator ?
sense.  The returned JSON documents will be parsed by the function of
one argument designated by FROM-JSON.  Requires a Postgres GIN index
without JSONB_PATH_OPS defined on MODEL."
  (log:trace "Call exists on ~A" model)
  (ensure-model-query model 'exists$)
  (ensure-transaction-level (contains read-committed-ro)
    (mapcar from-json (exists$ model property))))

(defun distinct (model property &key (from-json *from-json*))
  "Return all distinct values of the top level PROPERTY, a string, in
all of the JSON documents of MODEL, a symbol.  Every JSON document
must be a JSON object, with property PROPERTY defined.  So this
DISTINCT does not make sense if your JSON documents are arrays.  The
returned JSON documents wil parsed by the function of one argument
designated by FROM-JSON.  Note that this is _not_ a prepared query so
care must be taken that PROPERTY is sanitized if it derives from
arbitrary user input."
  (let ((query `(:select (j-> ,property)
                 :distinct
                 :from ',model)))
    (ensure-transaction-level (filter read-committed-ro)
      (mapcar from-json
              (query (sql-compile (json-query-to-s-sql query))
                     :column)))))

(defun history (model key &key (from-json *from-json*) (validity-keys-p t)
                               (valid-from-key "_validFrom") (valid-to-key "_validTo"))
  "Returns a list, in chronological order, of all previous values of
the object with primary key KEY, of type compatible with Postgres type
KEY-TYPE in the model's parameters, in MODEL, a symbol.  If such
objects exist return a parse of each JSON string by the function of
one argument designated by FROM-JSON.  If the object has no history,
return NIL.  If VALIDITY-KEYS-P is true, include the valid-from and
valid-to Postgres timestamps for the historical object.
VALID-FROM-KEY and VALID-TO-KEY are strings that will be the key names
of the respective timestamps."
  (log:debu4 "List history of object with key ~A from ~A" key model)
  (ensure-model-query model 'history$)
  (let ((rows (ensure-transaction-level (get read-committed-ro)
                  (history$ model key))))
    (loop for (jdoc valid-from valid-to) in rows
          for obj = (funcall from-json jdoc)
          when validity-keys-p
            do (setf (gethash valid-from-key obj) valid-from)
               (setf (gethash valid-to-key obj) valid-to)
          collect obj)))
