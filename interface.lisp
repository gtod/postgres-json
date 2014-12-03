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
      (ensure-transaction-level (create-backend repeatable-read-rw)
        (pomo:create-schema *pgj-schema*)
        (create-db-sequence *pgj-sequence* *pgj-schema*)
        (create-model *meta-model* (meta-model-parameters))
        *pgj-schema*)))

(defun backend-exists-p ()
  "Does the backend *PGJ-SCHEMA* exist?"
  (pomo:schema-exist-p *pgj-schema*))

(defun create-model (model &optional (parameters (make-model-parameters model)))
  "Create the PostgreSQL tables and indexes for PostgreSQL JSON
persistence model MODEL, a symbol.  Uses the various values in the
PARAMETERS has table to customize the model.  Should only be called
once per model.  Returns MODEL."
  (let* ((base model)
         (base-old (sym-suffix base "old"))
         (index (sym-suffix base "gin"))
         (index-old (sym-suffix base "old-gin")))
    (ensure-transaction-level (create-model repeatable-read-rw)
      (create-base-table base parameters)
      (create-old-table base-old parameters)
      (when (eq 'jsonb (jdoc-type parameters))
        (create-gin-index index base parameters)
        (create-gin-index index-old base-old parameters))
      (unless (eq *meta-model* model)
        (insert *meta-model* parameters :use-key (symbol->json model))))
    model))

(defun model-exists-p (model)
  "Does MODEL, a symbol, exist in our backend?"
  (if (get *meta-model* (symbol->json model)) t nil))

(defun all-models ()
  "Return a list of all models in the backend."
  (mapcar #'json->symbol (keys *meta-model*)))

(defun drop-backend! ()
  "Drop the backend (that is the PostgreSQL schema *PGJ-SCHEMA*) in
the database Postmodern is currently connected to.  This will
irrevocably delete ALL your data in ALL your models so it uses
a RESTART-CASE to guard against human error."
  (flet ((drop ()
           (drop-db-schema-cascade *pgj-schema*)))
    (let ((attempted-to (format nil "DROP all models' data(!) in schema: ~A" *pgj-schema*)))
      (restart-case (error 'database-safety-net
                           :attempted-to attempted-to
                           :suggestion "Pick an appropriate restart")
        (cancel () :report "Leave this schema alone." (return-from drop-backend! nil))
        (really-do-it () :report "I really want to drop ALL data in ALL models(!)" (drop))))))

(defun drop-model! (model)
  "Drop model MODEL.  This will irrevocably delete all data associated
with the model so it uses a RESTART-CASE to guard against human
error."
  (flet ((drop ()
           (ensure-transaction-level (drop-model! repeatable-read-rw)
             (delete *meta-model* (symbol->json model))
             (drop-db-table-cascade model *pgj-schema*)
             (drop-db-table-cascade (sym t model "-old") *pgj-schema*))))
    (restart-case (error 'database-safety-net
                         :attempted-to (format nil "DROP model ~A" model)
                         :suggestion "Pick an appropriate restart")
      (cancel () :report "Leave this model alone." (return-from drop-model! nil))
      (really-do-it () :report "I really want to DROP this model" (drop)))))

(defun flush-prepared-queries ()
  "If you get a 'Database error 26000: prepared statement ... does not
exist error' while mucking around at the REPL, call this.  A similar
error in production code should be investigated."
  (setf *query-functions* (make-hash-table :test #'equal)))
