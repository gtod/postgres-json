(in-package :postgres-json)

(defun create-base-table (name schema)
  "Create a PostgreSQL table called NAME in SCHEMA, both symbols.
This is the base table for the model.  Requires an active DB
connection."
  (run `(:create-table ,(qualified-name-string name schema)
         ((,*id*      :type ,*id-type*   :primary-key t)
          (valid-to   :type timestamptz :default (:type "infinity" timestamptz))
          (valid-from :type timestamptz :default (:transaction-timestamp))
          (,*jdoc*    :type ,*jdoc-type*)))))

(defun create-old-table (name schema)
  "Create a PostgreSQL table called NAME in SCHEMA, both symbols.
This is the 'old' table for the model, which will store non current
rows.  Requires an active DB connection."
  (run `(:create-table ,(qualified-name-string name schema)
         ((,*id*      :type ,*id-type*)
          (valid-to   :type timestamptz)
          (valid-from :type timestamptz)
          (,*jdoc*    :type ,*jdoc-type*))
         (:primary-key ,*id* valid-to))))

(defun create-gin-index (name table schema)
  "Create a PostgreSQL GIN index with NAME on a database table TABLE
in SCHEMA, all symbols.  Note that if you use *JDOC-TYPE* 'json when
creating the tables you cannot then create these indexes --- see the
PostgreSQL documentation.  In fact the option 'jsonb_path_ops' to the
GIN index may be desirable under some circumstances but I have not yet
twisted s-sql to generate such an index declaration (clearly we could
just use a string).  Requires an active DB connection."
  (run `(:create-index ,name :on ,(qualified-name-string table schema)
         :using gin :fields ,*jdoc*)))
