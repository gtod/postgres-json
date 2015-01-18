;;;; This file does not contain all the specials in the library.
;;;; Those closeley tied to implementaion code sit in those files.

(in-package :postgres-json)

(defvar *pgj-schema* 'pgj-model
  "A symbol being the name of the Postgres schema created to house all
database backend objects.")

(defvar *pgj-sequence* 'pgj-seq
  "A symbol being the name of the default Postgres sequence created to
automatically generate primary keys for JSON documents inserted into a
backend model.")

;; I think it sort of makes sense not to sleep at all for the first
;; retry, but then to back off pretty fast.  But I am no expert...
(defvar *serialization-failure-sleep-times* '(0 1 2 4 7)
  "The length of this list of real numbers determines the number of
times to retry when a Postgres transaction COMMIT sees a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE condition.  For each retry we
sleep the duration specified plus a random number of milliseconds
between 0 and 2000.  However, if 0 sleep is specified, we do not sleep
at all.  If set to NIL no condition handling is performed hence the
client will always see any such serialization failures.")

;; We are not locking write access to this hash so it is not thread
;; safe.  See postgres-json-parallel.asd and parallel.lisp for one
;; solution...
(defvar *query-functions* (make-hash-table :test #'equal)
  "Hash of schema/model/query-name => query function.")

(defvar *from-json* #'from-json
  "A function designator for a function of one argument which returns
the result of parsing the JSON string being its input.")

(defvar *to-json* #'to-json
  "A function designator for a function of one argument which
serializes a lisp object to a JSON string.")
