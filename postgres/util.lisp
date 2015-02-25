(in-package :postgres-json)

(defvar *debug-sql* nil
  "Set true to inspect S-SQL forms sent to RUN, instead of compiling
and executing.")

;;;; Qualified PostgreSQL table names using Postmodern's S-SQL

(defun qualified-name (name &optional (schema *pgj-schema*))
  "Return the S-SQL :dot form of NAME and SCHEMA, both symbols."
  `(:dot ',schema ',name))

(defun qualified-name-string (name &optional (schema *pgj-schema*))
  "Return a string of the Postgres 'qualified name' of NAME and SCHEMA,
both symbols."
  (sql-compile (qualified-name name schema)))

;;;; Utility

(defgeneric pomo-timestamp-to-string (stamp)
  (:method ((stamp string))
    stamp)
  ;; I don't want simple-date as a dependency but you might need it.
  ;; See model interface function HISTORY, for example.
  ;; And see system postgres-json-time.asd
  ;; (:method ((stamp simple-date:timestamp))
  ;;   (pomo-timestamp-to-string
  ;;    (local-time:universal-to-timestamp
  ;;     (simple-date:timestamp-to-universal-time stamp))))
  (:documentation "When systems such as simple-date or
cl-postgres+local-time are used they will return their respective
timestamp types for Postgres timestamps.  This generic function
ensures the timestamp ends up as a string, ie JSON."))

(defun run (form)
  "Compile and then run the S-SQL form FORM, unless *DEBUG-SQL* is true
is which case just PRINT the FORM."
  (if *debug-sql*
      (print form)
      (if (stringp form)
          (query form)
          (query (sql-compile form)))))

