(in-package :postgres-json)

;;;; Model backend specials

;;; Thse specials are effectively constants.  You _could_ rebind them
;;; but you would need to do it for every use of every interface
;;; function.  If you do need different values you would be better of
;;; just redefining these in some file in your own project.

(defvar *pgj-schema* 'pgj-model
  "A symbol being the name of the PostgreSQL schema we create to house
all database backend objects.")

;; No reason for user to change this, it sits in a fresh schema we made.
(defvar *pgj-sequence* 'pgj-seq
  "A symbol being the name of the PostgreSQL sequence we create
for (at least) the use of of meta model.")

(defvar *meta-model* 'pgj-meta
  "A symbol being the name of the model in which we store meta data
relating to user models.")

;;;; Export specials for client rebinding

(defvar *to-json* 'to-json
  "Function designator for function of one argument to serialize lisp
objects (submitted to INSERT and UPDATE, for example) to JSON.  Bind
it at run time for use by the model interface functions.  Or redefine
it globally for use in your own project.")

(defvar *from-json* 'yason:parse
  "Function designator for function of one argument to make lisp
objects from JSON strings retrieved from the DB backend.  Used by GET,
for example.  Bind it at run time for use by the model interface
functions.  Or redefine it globally for use in your own project.")

(defvar *db-handle-serialization-failure-p* t
  "UPDATE and DELETE calls on the model will use the Postgres
'repeatable read isolation level' so 'serialization failures' may
occur.  When this special variable is set to T (the default), these
failures are handled under the covers.  (However, if excessive time
elapses, client code may still see a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE).  If you would rather
explicitly handle _all_ serialization failures in your client code,
set this to NIL.")

;; I think it sort of makes sense not to sleep at all for the first
;; retry, but then to back off pretty fast.  But I am no expert...
(defvar *serialization-failure-sleep-times* '(0 1 2 4 7)
  "The length of this list of real numbers determines the number of
times to retry when a Postgres transaction COMMIT see a
CL-POSTGRES-ERROR:SERIALIZATION-FAILURE condition.  For each retry we
sleep the duration specified, plus a random number of milliseconds
between 0 and 2000.  However, if 0 sleep is specified, we do not sleep
at all.")
