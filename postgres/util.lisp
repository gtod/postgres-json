(in-package :postgres-json)

(defvar *debug-sql* nil
  "Set true to inspect S-SQL forms sent to RUN, instead of compiling
and executing.")

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
