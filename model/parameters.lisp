(in-package :postgres-json)

;;;; These are the default model parameters, their sole purpose is to
;;;; prvide the default values for MAKE-MODEL-PARAMETERS.

(defvar *sequence* 'pgj-seq
  "A symbol being the default name of the PostgreSQL sequence to
provide unique IDs for JSON objects inserted into the PostgreSQL
backend base table.")

(defvar *id* 'id
  "A symbol being the name of the primary key column in backend
tables.")

(defvar *id-type* 'integer
  "A symbol being the type of the primary key column in backend
tables.")

(defvar *jdoc* 'jdoc
  "A symbol being the name of the JSON column in created backend
tables.")

(defvar *jdoc-type* 'jsonb
  "A symbol being the type of the JSON column in created backend
tables.")

;; I get nervous when I see writers or accessors or any setfing of
;; slot values.  Objects of this class, once created, are read only.
(defclass model-parameters ()
  ((model :initarg :model :type symbol :reader model)
   (sequence :initarg :sequence :type symbol :reader sequence)
   (id :initarg :id :type symbol :reader id)
   (id-type :initarg :id-type :type symbol :reader id-type)
   (jdoc :initarg :jdoc :type symbol :reader jdoc)
   (jdoc-type :initarg :jdoc-type :type symbol :reader jdoc-type))
  (:documentation "A class to facilitation customization of backend
features of a model.  We need consistency between calls to
CREATE-MODEL and the functions that make the prepared queries for a
given model under the covers (say when INSERT is called).  By
serializing objects of this class to the DB we ensure that
consistency.  It also provides a simple example of using our JSON
persistence model. Slot type is used for JSON de/serialization."))

(defmethod table ((params model-parameters))
  (qualified-name (model params) *pgj-schema*))

(defmethod table-old ((params model-parameters))
  (qualified-name (sym-suffix (model params) "old") *pgj-schema*))

(defun make-model-parameters (model &key (sequence *pgj-sequence*)
                                         (id *id*) (id-type *id-type*)
                                         (jdoc *jdoc*) (jdoc-type *jdoc-type*))
  "Create an object of class MODEL-PARAMETERS to specify backend
features of a PostgreSQL JSON persistence model MODEL (a symbol),
typically to be supplied to CREATE-MODEL.  For each keyword argument
which defaults to a special variable see the documentation of that
variable."
  (make-instance 'model-parameters :model model :sequence sequence
                 :id id :id-type id-type :jdoc jdoc :jdoc-type jdoc-type))

;;;; The specific parameters for our meta model

(defun meta-model-parameters ()
  "Eating our own dog food, we keep the model parameters for all user
models in a 'meta' model, which itself has the following parameters."
  (make-model-parameters *meta-model* :id 'model :id-type 'text))
