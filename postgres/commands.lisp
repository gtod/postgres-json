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

(defun drop-db-table-cascade (table)
  "Drop a Postgres TABLE, a string, and all dependent views,
indexes etc.  Use with care."
  (run (format nil "drop table ~A cascade" table)))

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

(defun %table-exists-p (table)
  "Does TABLE, a string, exist in the Postgres backend?"
  (let ((query (sql-compile `(:select (:type ,table regclass)))))
    (first-value (ignore-errors (query query :single)))))

(defun flush-prepared-queries ()
  "If you get a 'Database error 26000: prepared statement ... does not
exist error' while mucking around at the REPL, call this.  A similar
error in production code should be investigated."
  (setf *query-functions* (make-hash-table :test #'equal)))
