(defpackage :postgres-json
  (:use :cl :alexandria :postmodern :s-sql)
  (:shadow :get :delete)
  (:export 
   :*db-schema*
   :*db-sequence*
   :*db-handle-serialization-failure-p*
   :*serialization-failure-sleep-times*
   :def-model-package
   :drop-schema-cascade
   :create-default-db-schema
   :create-default-db-sequence
   :create-model-backend
   :bake-model))
