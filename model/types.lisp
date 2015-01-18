;;;; Types

;;; There are no slots by design, the focus is on types not state.
;;; Because there are no slots, one instance is identical to another
;;; so we can use a single global instance if desired --- see
;;; DEFINE-GLOBAL-MODEL.

;;; There's a danger of combinatorial explosion below but it seems a
;;; good idea to use types to signify intent where possible.  See, for
;;; example, HAVING-PROPERTY and ENUMERATE-PROPERTY.  Only types
;;; currently in use are documented.

;;; Clients will typically subclass either PGJ-OBJECT-MODEL or
;;; PGJ-HISTORY-OBJECT-MODEL and store JSON objects in their models.
;;; However most interface methods are specialized only on the base
;;; class PGJ-MODEL so it's also simple to store JSON numbers, strings
;;; or arrays in a model.

(in-package :postgres-json)

;;;; JSON document types

;; The nouns in the doc strings below such as array, object,
;; structure, number and string are used in their JSON sense.
(defclass json-document () ()
  (:documentation "Base class for types of JSON documents."))

(defclass scalar-jdoc (json-document) ())
(defclass string-jdoc (scalar-jdoc) ())
(defclass number-jdoc (scalar-jdoc) ())
(defclass structure-jdoc (json-document) ())
(defclass object-jdoc (structure-jdoc) ())
(defclass array-jdoc (structure-jdoc) ())

;;;; Postgres-JSON model types

(defclass pgj-model () ()
  (:documentation "The Postgres-JSON model base class supported by
implementation and interface methods for storing, querying and
modifying JSON documents in a Postgres database."))

(defclass pgj-structure-model (pgj-model structure-jdoc) ()
  (:documentation "A Postgres-JSON model that consists of JSON
documents having either an object or array root node."))

(defclass pgj-object-model (pgj-structure-model) ()
  (:documentation "A Postgres-JSON model that consists of JSON
documents having an object root node."))

(defclass pgj-array-model (pgj-structure-model) ())

;;;; Postgres-JSON model history types

(defclass pgj-history-model (pgj-model) ()
  (:documentation "A Postgres-JSON model that maintains a history of
previous values of updated or deleted documents."))

(defclass pgj-history-structure-model (pgj-history-model pgj-structure-model) ())

(defclass pgj-history-object-model (pgj-history-model pgj-object-model) ()
  (:documentation "A Postgres-JSON model that maintains history and
consists of JSON documents having an object root node."))

(defclass pgj-history-array-model (pgj-history-model pgj-array-model) ())
