(in-package :postgres-json)

;;;; Implementation

(defun create-pgj-schema ()
  "Create the schema *PGJ-SCHEMA* and other backend objects needed to
house user created PostgreSQL JSON persistence models.  Call just once
in a given PostgreSQL database."
  (maybe-transaction (create-backend read-committed-rw)
    (pomo:create-schema *pgj-schema*)
    (create-db-sequence *pgj-sequence* *pgj-schema*)
    *pgj-schema*))

(defun pgj-schema-exists-p ()
  "Does the backend *PGJ-SCHEMA* exist?"
  (pomo:schema-exist-p *pgj-schema*))

(defun ensure-pgj-schema ()
  "Call CREATE-PGJ-SCHEMA unless the Postgres *PGJ-SCHEMA* already exists."
  (unless (pgj-schema-exists-p)
    (create-pgj-schema)))

;;;; Interface

(defvar *postmodern-connection* nil
  "Set this to a list congruent with the parameters expected by
POSTMODERN:CONNECT-TOPLEVEL, for use by the testing and example
code.")

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

(defun drop-pgj-schema ()
  "Drop the entire Postgres schema *PGJ-SCHEMA* in the database
Postmodern is currently connected to.  This will irrevocably delete
ALL your data in ALL your models so it uses a RESTART-CASE to guard
against human error."
  (flet ((drop ()
           (maybe-transaction (drop-backend read-committed-rw)
             (drop-db-schema-cascade *pgj-schema*))))
    (when (pgj-schema-exists-p)
      (let ((attempted-to (format nil "DROP all models' data(!) in schema: ~A" *pgj-schema*)))
        (restart-case (error 'database-safety-net
                             :attempted-to attempted-to
                             :suggestion "Pick an appropriate restart")
          (cancel () :report "Leave this schema alone." (return-from drop-pgj-schema nil))
          (really-do-it () :report "I really want to drop ALL data in ALL models(!)" (drop)))))))

;; I _hate_ this search_path nonsense and have largely avoided it by
;; using fully qualified relation names.  But DEFINE-MODEL-QUERY
;; presents a challege I have not yet surmounted (we want to write
;; 'cat not 'pgj_model.cat).  In the mean time we have this...  If
;; you don't know anything about search paths this should do the job,
;; and if you do you can look after yourself.
(defvar *default-search-path* (format nil "~A,public" (to-sql-name *pgj-schema*))
  "The default value used by ALTER-ROLE-SET-SEARCH-PATH.")

(defun alter-role-set-search-path (user &optional (search-path *default-search-path*))
  "Alter the role of Postgres user USER, a string, to set the
'search_path' setting to the string SEARCH-PATH.  In most cases this
is what you want so than when defining your own queries with
DEFINE-MODEL-QUERY unqualified relation names can be found in our
default schema (which is not the PUBLIC schema).  This setting does
_not_ effect the normal model interface functions such as FETCH and
FILTER as they use fully qualified table names at all times.  Will
only take effect upon your next connection.  Beware, may be overridden
by settings in your ~/.psqlrc file.  See also the Postgres
documentation on search paths and settings."
  (query (format nil "ALTER ROLE ~A SET search_path TO ~A" user search-path)))
