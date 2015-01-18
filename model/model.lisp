(in-package :postgres-json)

;;;; Model properties

(defgeneric model-name (model)
  (:documentation "The symbol being the proper name of the
MODEL.")
  (:method ((model pgj-model))
    (type-of model)))

(defgeneric model-table (model)
  (:documentation "The Postgres qualified name as an S-SQL form for
the base table of MODEL.")
  (:method ((model pgj-model))
    (qualified-name (model-name model))))

(defgeneric model-sequence (model)
  (:documentation "The name, a symbol, of a Postgres sequence to
provide primary keys upon insertion of fresh documents into a backend
model.  May be NIL, in which case explicit primary keys must be
supplied for all inserts.")
  (:method ((model pgj-model))
    *pgj-sequence*))

;; Compound keys?
(defgeneric model-key-name (model)
  (:documentation "The name, a symbol, for the primary key column in
backend model tables.")
  (:method ((model pgj-model))
    'key))

(defgeneric model-key-type (model)
  (:documentation "The name, a symbol, for the Postgres type of the
primary key column in the backend model tables.  KEY arguments to
model interface methods must be compatible with this type.")
  (:method ((model pgj-model))
    'integer))

(defgeneric model-initial-gin-operator-class (model)
  (:documentation "The name, a keyword, for the initial Postgres GIN
operator class to use for the model's GIN index.  See also
USE-GIN-INDEX.  If NIL, make no GIN index.")
  (:method ((model pgj-model))
    :jsonb-ops))

;;;; Model GIN indexes

;; See Postgres manual 9.4, 8.14.4.
;; Choices for gin-operator-class are jsonb_ops and jsonb_path_ops.
;; The later is smaller and faster but does not support the existence
;; operator: ?

;; In fact we can create a simple BTREE index too:
;; CREATE INDEX geodata_index ON
;;    geodata_json ((data->>'country_code'), (data->>'asciiname'));
;; which will be smaller but you need to explicitly list the keys to index and
;; it does not support @>

;; It's easy to test and change indexes at run time:
;; explain analyze select jdoc from booking where jdoc @> '{"state": "pending"}';
;; drop index booking_gin;
;; create index booking_gin on booking using GIN (jdoc jsonb_path_ops);
;; etc...

(defparameter *gin-operator-classes* '(:jsonb-ops :jsonb-path-ops)
  "A list of keywords representing Postgres GIN operator classes.")

(defgeneric use-gin-index (model gin-operator-class)
  (:documentation "Create a Postgres GIN index for MODEL using
GIN-OPERATOR-CLASS, a keyword that must be a member of
*gin-operator-classes*.  First drop any existing GIN index.")
  (:method :before ((model pgj-model) gin-operator-class)
    (assert (member gin-operator-class *gin-operator-classes*)))
  (:method ((model pgj-model) gin-operator-class)
    (let ((table (sql-compile (model-table model)))
          (index-name (sym-suffix (model-name model) "gin"))
          (op-class (to-sql-name gin-operator-class)))
      (maybe-transaction (use-gin-index read-committed-rw)
        (handler-case (run `(:drop-index :if-exists ,index-name))
          (warning () nil))
        (run `(:create-index ,index-name :on ,table
               :using gin :fields (:raw ,(format nil "jdoc ~A" op-class))))))))

;;;; Model backend

(defgeneric create-base-table (model)
  (:documentation "Create a Postgres table to contain JSON documents
for MODEL.")
  (:method ((model pgj-model))
    (let ((table (sql-compile (model-table model)))
          (key-name (model-key-name model))
          (key-type (model-key-type model)))
      (run `(:create-table ,table
           ((,key-name  :type ,key-type :primary-key t)
            (valid-to   :type timestamptz :default (:type "infinity" timestamptz))
            (valid-from :type timestamptz :default (:transaction-timestamp))
            (jdoc       :type jsonb)))))))

(defgeneric create-backend (model)
  (:documentation "Create the backend tables and indexes for a
MODEL.")
  (:method :around ((model pgj-model))
    (maybe-transaction (create-backend read-committed-rw)
      (ensure-pgj-schema)
      (call-next-method)))
  (:method ((model pgj-model))
    (create-base-table model)
    (when-let ((op-class (model-initial-gin-operator-class model)))
      (use-gin-index model op-class))))

(defgeneric backend-exists-p (model)
  (:documentation "Return true if MODEL has a Postgres backend, NIL
otherwise.")
  (:method ((model pgj-model))
    (%table-exists-p (sql-compile (model-table model)))))

(defgeneric ensure-backend (model)
  (:documentation "Call CREATE-BACKEND on MODEL unless said backend
already exists.")
  (:method ((model pgj-model))
    (unless (backend-exists-p model)
      (create-backend model))))

(defgeneric drop-backend (model)
  (:documentation "Drop the Postgres backend of MODEL.  This will
irrevocably delete all data associated with the model.")
  (:method :around ((model pgj-model))
    (flet ((drop ()
             (maybe-transaction (drop-backend read-committed-rw)
               (call-next-method))))
      (restart-case (error 'database-safety-net
                           :attempted-to (format nil "DROP model ~A" (model-name model))
                           :suggestion "Pick an appropriate restart")
        (cancel () :report "Leave this model alone.")
        (really-do-it () :report "I really want to DROP this model's backend." (drop)))))
  (:method ((model pgj-model))
    (drop-db-table-cascade (sql-compile (model-table model)))))

;;;; JSON de/serialization

;;; These two methods are specifically for *entire JSON documents*
;;; going to/from the model's backend.  It may be that validation is
;;; performed before serialization or derived keys inserted after
;;; deserialization.  When interface methods need to convert to/from
;;; JSON for other uses cases (see FILTER, for example) then *TO-JSON*
;;; and *FROM-JSON* are used...

(defgeneric serialize (model object)
  (:documentation "Serialize lisp OBJECT to a form suitable for
storage as a JSON document in backend MODEL.  Return same.  Called by
INSERT, for example, to convert Lisp objects to JSON before DB
insertion proper.")
  (:method ((model pgj-model) object)
    (funcall *to-json* object)))

(defgeneric deserialize (model jdoc)
  (:documentation "Deserialize the string JDOC from MODEL's backend to
a lisp object.  Return same.  Called by FETCH, for example, to convert
JSON strings from the backend into Lisp objects.")
  (:method ((model pgj-model) (jdoc string))
    (funcall *from-json* jdoc)))

(defgeneric stash (model object key)
  (:documentation "Called before SERIALIZE which is called before
document inserts or updates.  An opportunity to modify the lisp OBJECT
using the intended/current primary KEY of the JSON document in the
MODEL's backend.")
  (:method ((model pgj-model) object key)
    "Do nothing and return OBJECT."
    (declare (ignore key))
    object)
  (:method ((model pgj-object-model) (object hash-table) key)
    "Destructively modify hash-table OBJECT by assigning the value KEY
to a key named by the downcased symbol name of MODEL-KEY-NAME of
MODEL.  Returns the modified OBJECT."
    (let ((key-name (string-downcase (symbol-name (model-key-name model)))))
      (setf (gethash key-name object) key)
      object)))

;;;; Interface

;; By design DEFINE-GLOBAL-VAR* symbols should not be rebound
(defmacro define-global-model (name constant (&rest superclasses))
  "Define a new class named NAME, a symbol, having SUPERCLASSES, all
symbols.  Define a global variable named CONSTANT, a symbol, with
value an instance of the new class."
  `(progn
     (defclass ,name (,@superclasses) ())
     (define-global-var* ,constant (make-instance ',name))))
