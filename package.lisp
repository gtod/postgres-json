(defpackage :postgres-json
  (:use :cl :alexandria :postmodern :s-sql)
  (:shadow :get :delete)
  (:export

   ;; Specials
   :*db-schema*
   :*db-sequence*
   :*to-json*
   :*from-json*
   :*db-handle-serialization-failure-p*
   :*serialization-failure-sleep-times*

   ;; PostgreSQL
   :database-safety-net
   :create-db-schema
   :create-db-sequence
   :drop-db-schema-cascade

   ;; Setup a model
   :create-model-backend
   :bake-model

   ;; Model user interface
   :insert
   :update
   :get
   :delete
   :keys))

;;; Also some convenience functions (not exported) that you may wish to import:
;;; (:import-from :postgres-json :obj :pp-json)
