(in-package :postgres-json)

;;;; Define the CRUD++ interface to the Postgre-JSON persistence model

(defgeneric insert (model object &optional key)
  (:documentation "Insert lisp object OBJECT into the backend MODEL,
after JSON serialization.  If KEY is supplied use that as the primary
key for the JSON document rather than an automatically generated one.
Return the new primary key.")
  (:method ((model pgj-model) object &optional key)
    (maybe-transaction (insert +read-committed-rw+)
      (let ((key (or key (nextval-sequence$ model))))
        (let ((object (stash model object key)))
          (first-value (insert$ model key (serialize model object))))))))

;; We also need a MERGE, or MIXIN or UPDATE of some sort. The Postgres
;; people may help us with the next release or there are suggestions
;; on stackexchange...
(defgeneric supersede (model key object)
  (:documentation "Replace the current value of the JSON document
having primary key KEY in MODEL with the JSON serialization of lisp
object OBJECT.  Return KEY on success, NIL if no such KEY is found.")
  (:method ((model pgj-model) key object)
    (maybe-transaction (supersede +read-committed-rw+)
      (let ((object (stash model object key)))
        (first-value (supersede$ model key (serialize model object)))))))

(defgeneric fetch (model key)
  (:documentation "If there is a JSON document with primary key KEY in
MODEL return the result of deserializing it.  Otherwise return NIL.")
  (:method ((model pgj-model) key)
    (let ((jdoc (maybe-transaction (fetch +read-committed-ro+)
                  (fetch$ model key))))
      (if jdoc (deserialize model jdoc) nil))))

(defgeneric fetch-all (model)
  (:documentation "Return as a list the result of deserializing all
JSON documents in MODEL.")
  (:method ((model pgj-model))
    (maybe-transaction (fetch-all +read-committed-ro+)
      (mapcar (curry #'deserialize model) (fetch-all$ model)))))

(defgeneric excise (model key)
  (:documentation "Delete the JSON document with primary key KEY from
MODEL.  Return KEY on success, NIL if no such KEY exists.")
  (:method ((model pgj-model) key)
    (maybe-transaction (excise +read-committed-rw+)
      (first-value (excise$ model key)))))

(defgeneric excise-all (model)
  (:documentation "Delete all JSON documents in MODEL.  Returns the
number of documents deleted.")
  (:method ((model pgj-model))
    (maybe-transaction (excise-all +read-committed-rw+)
      (nth-value 1 (excise-all$ model)))))

(defgeneric keys (model)
  (:documentation "Return two values: a list of all primary keys for
MODEL and the length of that list.")
  (:method ((model pgj-model))
    (maybe-transaction (keys +read-committed-ro+)
      (keys$ model))))

(defgeneric tally (model)
  (:documentation "Return the count of all JSON documents in MODEL.")
  (:method ((model pgj-model))
    (maybe-transaction (count +read-committed-ro+)
      (first-value (tally$ model)))))

(defgeneric having-property (model property)
  (:documentation "Return the result of deserializing all JSON
documents in MODEL which have a top level object property PROPERTY, a
string, or if said string appears as an element of a top level array.
This is in the Postgres operator ?  sense.  Requires a Postgres GIN
index with operator class :jsonb-ops defined on MODEL.")
  (:method ((model pgj-structure-model) (property string))
    (maybe-transaction (contains +read-committed-ro+)
      (mapcar (curry #'deserialize model) (exists$ model property)))))

(defgeneric enumerate-property (model property)
  (:documentation "Return all distinct values of the top level
PROPERTY, a string, in all of the JSON documents of MODEL.  JSON
deserialization is performed by funcalling *FROM-JSON*.  Note that
this is _not_ a prepared query so care must be taken that PROPERTY is
sanitized if it derives from arbitrary user input.")
  (:method ((model pgj-object-model) (property string))
    (let ((query `(:select (j-> ,property)
                   :distinct
                   :from ,(model-table model))))
      (maybe-transaction (distinct +read-committed-ro+)
        (mapcar *from-json*
                (query (sql-compile (json-query-to-s-sql query))
                       :column))))))

(defgeneric contains (model contains &key)
  (:documentation "Filter all JSON documents in MODEL by checking they
'contain', in the Postgres @> operator sense, the object CONTAINS which
will be serialized to a JSON document by funcalling *TO-JSON*.  If
CONTAINS is NIL, apply no containment restriction.")
  (:method  ((model pgj-object-model) contains &key properties limit)
    "Filter all JSON documents in MODEL as follows.  Each document
must 'contain', in the Postgres @> operator sense, the object CONTAINS
which will be serialized to a JSON document by funcalling *TO-JSON*.
If CONTAINS is NIL, apply no containment restriction.  PROPERTIES may
be a list of strings being properties in the top level of the JSON
documents in MODEL and only the values of said properties will be
returned, bundled together in a JSON document.  If PROPERTIES is NIL
the entire JSON document will be returned.  LIMIT, if supplied, must
be an integer that represents the maximum number of objects that will
be returned.  If properties is NIL JSON deserialization is performed
by DESERILIZE, otherwise by funcalling *FROM-JSON*.  Note that this is
_not_ a prepared query so extra care must be taken if PROPERTIES or
CONTAIN derive from unsanitized user input."
    (let ((filter (if contains (funcall *to-json* contains) nil)))
      (let ((select `(:select ,(if properties `(jbuild ,properties) 'jdoc)
                      :from ,(model-table model)
                      :where ,(if filter `(:@> 'jdoc ,filter) "t"))))
        (let ((query (if (integerp limit) `(:limit ,select ,limit) select))
              (from-json (if properties *from-json* (curry #'deserialize model))))
          (maybe-transaction (filter +read-committed-ro+)
            (mapcar from-json
                    (query (sql-compile (json-query-to-s-sql query))
                           :column))))))))

;;;; History methods

(defmethod supersede ((model pgj-history-model) key object)
  "As per SUPERSEDE but keep a separate record of all previous rows."
  (declare (ignore object))
  (maybe-transaction (supersede-history +repeatable-read-rw+)
    (insert-old$ model key)
    (call-next-method)))

(defmethod excise ((model pgj-history-model) key)
  "As per EXCISE but keep a separate record of all deleted rows."
  (maybe-transaction (excise-history +repeatable-read-rw+)
    (insert-old$ model key)
    (call-next-method)))

(defmethod excise-all ((model pgj-history-model))
  "As per EXCISE-ALL but keep a separate record of all deleted rows."
  (maybe-transaction (excise-all-history +repeatable-read-rw+)
    (dolist (key (keys model))
      (excise model key))))

(defgeneric history (model key &key)
  (:documentation "Return a list of the result of deserializing all
previous values of the JSON document with primary key KEY in MODEL.")
  (:method ((model pgj-history-model) key
            &key (validity-keys-p t)
                 (valid-from-key "_validFrom") (valid-to-key "_validTo"))
    "Return a list of the result of deserializing all previous values
of the JSON document with primary key KEY in MODEL, in chronological
order.  If VALIDITY-KEYS-P is true, include the 'valid_from' and
'valid_to' Postgres timestamps for the historical document as
properties in the top level JSON object --- it must be an object in
this case.  VALID-FROM-KEY and VALID-TO-KEY are strings that will be
the property names of the respective timestamps."
    (let ((rows (maybe-transaction (history +read-committed-ro+)
                  (history$ model key))))
      (loop for (jdoc valid-from valid-to) in rows
            for obj = (deserialize model jdoc)
            when validity-keys-p
              do (setf (gethash valid-from-key obj)
                       (pomo-timestamp-to-string valid-from))
                 (setf (gethash valid-to-key obj)
                       (pomo-timestamp-to-string valid-to))
            collect obj))))
