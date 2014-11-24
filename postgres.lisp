(in-package :postgres-json)

(defvar *debug-sql* nil
  "Set true to inspect S-SQL forms sent to RUN, instead of compiling
and executing.")

;;;; High level DB operations
;;;; See also postmodern util.lisp

;;; We can get away with these not being prepared statments because (I
;;; imagine) they are infrequently used, and usually from the REPL.

(defun drop-schema-cascade (name)
  "Drop a PostgreSQL schema and cascade delete all contained DB
objects(!) with name NAME, a symbol.  Requires an active DB connection."
  (when (string-equal "public" (symbol-name name))
    (error 'database-safety-net
           :attempted-to "Drop schema PUBLIC"
           :suggestion "Try pomo:drop-schema"))
  (pomo:drop-schema name :cascade t)
  name)

;;; Could we use currval to tell if it exists or not?
(defun create-sequence (name schema)
  "Create a PostgreSQL sequence with NAME in SCHEMA (both symbols).
Requires an active DB connection."
  (run `(:create-sequence ,(qualified-name name schema)))
  name)

;;;; Qualified PostgreSQL table names using Postmodern's S-SQL

(defun qualified-name (name schema)
  `(:dot ',schema ',name))

(defun qualified-name-string (name schema)
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
