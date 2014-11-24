(defpackage :postgres-json
  (:use :cl :alexandria :postmodern :s-sql)
  (:shadow :get :delete)
  (:export 
   :*db-schema*
   :*db-sequence*
   :*db-handle-serialization-failure-p*
   :*serialization-failure-sleep-times*
   :def-model-package
   :create-default-schema
   :drop-schema-cascade
   :create-default-sequence
   :create-backend
   :bake-model))
