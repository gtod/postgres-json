(defpackage :postgres-json
  (:use :cl :alexandria :postmodern :s-sql)
  (:shadow :get :delete)
  (:export 
   :*db-schema*
   :*db-sequence*
   :create-default-schema
   :drop-schema-cascade!
   :create-default-sequence
   :create-backend
   :bake-interface))
