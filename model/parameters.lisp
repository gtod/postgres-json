(in-package :postgres-json)

;;;; These are the default model parameters, their sole purpose is to
;;;; provide the default values for MAKE-MODEL-PARAMETERS.

(defvar *sequence* 'pgj-seq
  "A symbol being the default name of the PostgreSQL sequence to
provide unique IDs for JSON objects inserted into the PostgreSQL
backend base table.")

(defvar *key* 'key
  "A symbol being the name of the primary key column in backend
tables.")

(defvar *key-type* 'integer
  "A symbol being the type of the primary key column in backend
tables.")

;; jsonb_path_ops is smaller and faster but does not support the
;; existence operator: ?
(defparameter *gin-operator-class* 'jsonb-ops
  "A symbol, which is TO-SQL-NAME converted to a string, being the
operator class of the GIN index created on the model relations.  See
Postgres 9.4 manual 8.14.4.")

;;;; The class proper and some reader methods

(defclass model-parameters (read-only)
  ((model :initarg :model :type symbol :reader model)
   (sequence :initarg :sequence :type symbol :reader sequence)
   (gin-operator-class :initarg :gin-operator-class :type string :reader gin-operator-class)
   (key :initarg :key :type symbol :reader key)
   (key-type :initarg :key-type :type symbol :reader key-type))
  (:documentation "A class to facilitate customization of backend
features of a model.  We need consistency between calls to
CREATE-MODEL and the functions that make the prepared queries for a
given model under the covers (say when INSERT is called).  By
serializing objects of this class to the DB we ensure that
consistency.  It also provides a simple example of using our JSON
persistence model. Slot :type should be included and is used for JSON
de/serialization."))

(defgeneric table (model-parameters)
  (:documentation "The Postgres S-SQL qualified table name for these
parameters."))

(defmethod table ((params model-parameters))
  (qualified-name (model params) *pgj-schema*))

(defgeneric table-old (model-parameters)
  (:documentation "The Postgres S-SQL qualified old table name for
these parameters."))

(defmethod table-old ((params model-parameters))
  (qualified-name (sym-suffix (model params) "old") *pgj-schema*))

;; The idea is that we can now write (see util for with-readers):
;; (with-readers (model table) (get-model-parameters 'foo)
;;   ; model, table are locally bound here to the result
;;   ; of calling their respective reader function.  So we don't
;;   ; care if it's a slot or a method.
;;   )

(defun make-model-parameters (model &key (sequence *pgj-sequence*)
                                         (gin-operator-class *gin-operator-class*)
                                         (key *key*) (key-type *key-type*))
  "Create an object of class MODEL-PARAMETERS to specify backend
features of a PostgreSQL JSON persistence model MODEL (a symbol),
typically to be supplied to CREATE-MODEL.  For each keyword argument
which defaults to a special variable see the documentation of that
variable."
  (make-instance 'model-parameters :model model :sequence sequence
                 :gin-operator-class gin-operator-class
                 :key key :key-type key-type))

;;; Insert

(defun insert-model-parameters (model-parameters)
  "Insert the MODEL-PARAMTERS object into the backend."
  (insert *meta-model* model-parameters :use-key (symbol->json (model model-parameters))))

;;; Get

(defun get-model-parameters (model)
  "Return the model-parameters object for MODEL, a symbol, by
retrieving them from the backend."
  (get *meta-model* (symbol->json model)
       :from-json 'model-parameters-from-json))

;;;; The specific parameters for our meta model

(defun meta-model-parameters ()
  "Eating our own dog food, we keep the model parameters for all user
models in a 'meta' model, which itself has the following parameters."
  (make-model-parameters *meta-model* :key 'model :key-type 'text))

(defun meta-model-p (model)
  (eq *meta-model* model))

;;;; JSON de/serialization

;; It would be simpler to specialize a yason:encode method on symbol
;; but that might confuse clients if they have other ideas for such a
;; method.
(defmethod yason:encode ((params model-parameters) &optional stream)
  (yason:with-output (stream)
    (yason:with-object  ()
      (dolist (slot (slot-definitions params))
        (let* ((name (slot-name slot))
               (type (slot-type slot))
               (slot-value (slot-value params name)))
          (let ((value (typecase type
                         (symbol (symbol->json slot-value))
                         (t slot-value))))
            (yason:encode-object-element (symbol->json name) value)))))))

(defun model-parameters-from-json (json)
  (let ((obj (yason:parse json))
        (params (make-instance 'model-parameters)))
    (dolist (slot (slot-definitions params))
        (let* ((name (slot-name slot))
               (type (slot-type slot))
               (value (gethash (symbol->json name) obj)))
          (setf (slot-value params name) (typecase type
                                           (symbol (json->symbol value))
                                           (t value)))))
    params))
