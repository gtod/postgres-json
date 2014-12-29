(in-package :postgres-json)

;; I spent a long time letting users create and use arbitrary schema.
;; It's much simpler (for now) not too, and not to let them use the
;; public schema either...
(defun create-backend ()
  "Create the schema *PGJ-SCHEMA* and other backend objects needed to
house user created PostgreSQL JSON persistence models.  Call just once
in a given PostgreSQL database."
  (if (pomo:schema-exist-p *pgj-schema*)
      (error 'database-safety-net
             :attempted-to (format nil "Create the backend, when a schema called ~A already exists." *pgj-schema*)
             :suggestion (format nil "Check carefully what data is in ~A" *pgj-schema*))
      (with-model-transaction (create-backend)
        (pomo:create-schema *pgj-schema*)
        (create-db-sequence *pgj-sequence* *pgj-schema*)
        (create-model *meta-model* (meta-model-parameters))
        *pgj-schema*)))

(defun backend-exists-p ()
  "Does the backend *PGJ-SCHEMA* exist?"
  (pomo:schema-exist-p *pgj-schema*))

(defun ensure-backend ()
  "Call CREATE-BACKEND unless the Postgres backend already exists."
  (unless (backend-exists-p)
    (create-backend)))

;; I _hate_ this search_path nonsense and have largely avoided it by
;; using fully qualified relation names.  But DEFINE-MODEL-QUERY
;; presents a challege I have not yet surmounted (we want to write
;; 'cat not 'pgj_model.cat).  In the mean time we have this...  If
;; you don't know anything about search paths this should do the job,
;; and if you do you can look after yourself.
(defparameter *default-search-path* (format nil "~A,public" (to-sql-name *pgj-schema*))
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

(defun create-model (model &optional (parameters (make-model-parameters model)))
  "Create the PostgreSQL tables and indexes for Postgre JSON
persistence model MODEL, a symbol.  Uses the various values in the
PARAMETERS CLOS object to customize the model.  Should only be called
once per model.  Returns MODEL."
  (let* ((base model)
         (base-old (sym-suffix base "old"))
         (index (sym-suffix base "gin"))
         (index-old (sym-suffix base "old-gin")))
    (with-model-transaction (create-model)
      (create-base-table base parameters)
      (create-old-table base-old parameters)
      (create-gin-index index base parameters)
      (create-gin-index index-old base-old parameters)
      (unless (eq *meta-model* model)
        (insert-model-parameters parameters)))
    model))

(defun model-exists-p (model)
  "Does MODEL, a symbol, exist in our backend?"
  (if (fetch *meta-model* (symbol->json model)) t nil))

(defun ensure-model (model &optional (parameters (make-model-parameters model)))
  "Call CREATE-MODEL with MODEL and PARAMETERS as arguments, unless MODEL
already exists."
  (unless (model-exists-p model)
    (create-model model parameters)))

(defun all-models ()
  "Return a list of all models in the backend."
  (mapcar #'json->symbol (keys *meta-model*)))

(defun drop-backend ()
  "Drop the backend (that is the PostgreSQL schema *PGJ-SCHEMA*) in
the database Postmodern is currently connected to.  This will
irrevocably delete ALL your data in ALL your models so it uses
a RESTART-CASE to guard against human error."
  (flet ((drop ()
           (with-model-transaction (drop-backend)
             (drop-db-schema-cascade *pgj-schema*))))
    (when (backend-exists-p)
      (let ((attempted-to (format nil "DROP all models' data(!) in schema: ~A" *pgj-schema*)))
        (restart-case (error 'database-safety-net
                             :attempted-to attempted-to
                             :suggestion "Pick an appropriate restart")
          (cancel () :report "Leave this schema alone." (return-from drop-backend nil))
          (really-do-it () :report "I really want to drop ALL data in ALL models(!)" (drop)))))))

(defun drop-model (model)
  "Drop model MODEL.  This will irrevocably delete all data associated
with the model so it uses a RESTART-CASE to guard against human
error."
  (flet ((drop ()
           (with-model-transaction (drop-model)
             (excise *meta-model* (symbol->json model))
             (drop-db-table-cascade model *pgj-schema*)
             (drop-db-table-cascade (sym t model "-old") *pgj-schema*))))
    (restart-case (error 'database-safety-net
                         :attempted-to (format nil "DROP model ~A" model)
                         :suggestion "Pick an appropriate restart")
      (cancel () :report "Leave this model alone." (return-from drop-model nil))
      (really-do-it () :report "I really want to DROP this model" (drop)))))

(defun flush-prepared-queries ()
  "If you get a 'Database error 26000: prepared statement ... does not
exist error' while mucking around at the REPL, call this.  A similar
error in production code should be investigated."
  (setf *query-functions* (make-hash-table :test #'equal)))
