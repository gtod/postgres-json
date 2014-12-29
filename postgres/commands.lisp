(in-package :postgres-json)

;;;; High level DB operations
;;;; See also postmodern util.lisp

;;; We can get away with these not being prepared statments because (I
;;; imagine) they are infrequently used, and usually from the REPL.

;; We could use the pomo:sequence-exists-p but that checks in _all_
;; schemas which is not really what we want.  Just let them see the
;; error...
(defun create-db-sequence (sequence &optional (schema *pgj-schema*))
  "Create a PostgreSQL sequence with name SEQUENCE in SCHEMA (both symbols).
Requires an active DB connection."
  (run `(:create-sequence ,(qualified-name sequence schema)))
  (values))

(defun drop-db-table-cascade (table schema)
  "Drop a PostgreSQL TABLE in SCHEMA (both symbols) and all dependent views,
indexes etc.  Use with care."
  (run (format nil "drop table ~A cascade" (qualified-name-string table schema))))

(defun drop-db-schema-cascade (schema)
  "Drop a PostgreSQL schema and cascade delete all contained DB
objects(!) with name SCHEMA, a symbol.  Requires an active DB
connection."
  (when (string-equal "public" (symbol-name schema))
    (error 'database-safety-net
           :attempted-to "Drop schema PUBLIC"
           :suggestion "Try pomo:drop-schema"))
  (pomo:drop-schema schema :cascade t)
  (values))

(defun ensure-top-level-connection (&optional (connect-spec *postmodern-connection*))
  "Ensure a Postmodern top level connection is active by applying the
contents of the list CONNECT-SPEC to POMO:CONNECT-TOPLEVEL."
  (unless connect-spec
    (error "Try setting POSTGRES-JSON:*POSTMODERN-CONNECTION* to a
list congruent with the parameters expected by POSTMODERN:CONNECT-TOPLEVEL.
For example: '\(\"mydb\", \"myusername\", \"\", \"localhost\"\).
This connection list is used by the example and testing code."))
  (unless (and pomo:*database* (pomo:connected-p pomo:*database*))
    (apply #'pomo:connect-toplevel connect-spec)))
