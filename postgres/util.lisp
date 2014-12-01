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
both symbols."
  (format nil "~A.~A" (to-sql-name schema) (to-sql-name name)))

(defun db-name-string (name)
  "Return the 'qualfied name' of Postgres object with name NAME in
*PGJ-SCHEMA*, both symbols."
  (qualified-name-string name *pgj-schema*))

;;;; Utility

(defun run (form)
  "Compile and then run the S-SQL form FORM, unless *DEBUG-SQL* is true
is which case just PRINT the FORM."
  (if *debug-sql*
      (print form)
      (if (stringp form)
          (query form)
          (query (sql-compile form)))))

(defmacro with-db-schema ((schema) &body body)
  "Execute BODY forms with *PGJ-SCHEMA* bound to SCHEMA, a form
that should evaluate to a symbol."
  `(let ((*pgj-schema* ,schema))
     ,@body))
