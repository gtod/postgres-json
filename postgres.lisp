(in-package :postgres-json)

(defvar *debug-sql* nil
  "Set true to inspect S-SQL forms sent to RUN, instead of compiling
and executing.")

;;;; High level DB operations
;;;; See also postmodern util.lisp

;;; We can get away with these not being prepared statments because (I
;;; imagine) they are infrequently used, and usually from the REPL.

(defun create-db-schema (&key (schema *db-schema*))
  "Create a new PostgreSQL schema call SCHEMA, a symbol.  Requires an
active DB connection.

If you are using PSQL remember to do something like

  SET search_path TO <your_schema>, public;

to be able to see the tables, indexes, etc. in your new schema."
  (pomo:create-schema schema)
  (values))

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
