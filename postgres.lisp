(in-package :postgres-json)

(defvar *debug-sql* nil
  "Set true to inspect S-SQL forms sent to RUN, instead of compiling
and executing.")

;;;; High level DB operations
;;;; See also postmodern util.lisp

;;; We can get away with these not being prepared statments because (I
;;; imagine) they are infrequently used, and usually from the REPL.

(defun create-db-schema (name)
  "Create a new PostgreSQL schema call NAME, a symbol.  Requires an
active DB connection.

If you are using PSQL remember to do something like

  SET search_path TO <your_schema>, public;

to be able to see the tables, indexes, etc. in your new schema."
  (pomo:create-schema name)
  (values))

(defun create-default-db-schema ()
  "Create a PostgreSQL schema with name *DB-SCHEMA*.  Requires an
active DB connection."
  (create-db-schema *db-schema*))

;; We could use the pomo:sequence-exists-p but that checks in _all_
;; schemas which is not really what we want.  Just let them see the
;; error...
(defun create-db-sequence (name schema)
  "Create a PostgreSQL sequence with NAME in SCHEMA (both symbols).
Requires an active DB connection."
  (run `(:create-sequence ,(qualified-name name schema)))
  (values))

(defun create-default-db-sequence ()
  "Create a PostgreSQL sequence with name *DB-SEQUENCE* in *DB-SCHEMA*
to act as the source of unique ids across _all_ database model tables.
Requires an active DB connection."
  (create-db-sequence *db-sequence* *db-schema*))

(defun drop-db-schema-cascade (name)
  "Drop a PostgreSQL schema and cascade delete all contained DB
objects(!) with name NAME, a symbol.  Requires an active DB
connection."
  (when (string-equal "public" (symbol-name name))
    (error 'database-safety-net
           :attempted-to "Drop schema PUBLIC"
           :suggestion "Try pomo:drop-schema"))
  (pomo:drop-schema name :cascade t)
  (values))

;;;; Qualified PostgreSQL table names using Postmodern's S-SQL

(defun qualified-name (name schema)
  "Return the S-SQL :dot form of NAME and SCHEMA, both symbols."
  `(:dot ',schema ',name))

(defun qualified-name-string (name schema)
  "Return a string of the Postgres 'qualified name' of NAME and SCHEMA,
both strings"
  (format nil "~A.~A" (to-sql-name schema) (to-sql-name name)))

;;;; Utility

(defun run (form)
  "Compile and then run the S-SQL form FORM, unless *DEBUG-SQL* is true
is which case just PRINT the FORM."
  (if *debug-sql*
      (print form)
      (if (stringp form)
          (query form)
          (query (sql-compile form)))))
