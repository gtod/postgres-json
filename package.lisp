(defpackage :postgres-json
  (:use #:cl #:alexandria #:postmodern #:s-sql)
  (:shadow #:sequence)

  ;; Specials
  (:export
   #:*pgj-schema*
   #:*default-search-path*
   #:*to-json*
   #:*from-json*
   #:*stash-key*
   #:*db-handle-serialization-failure-p*
   #:*serialization-failure-sleep-times*)

  ;; Postgres backend interface
  (:export
   #:database-safety-net
   #:really-do-it
   #:create-db-sequence
   #:create-backend
   #:backend-exists-p
   #:ensure-backend
   #:drop-backend
   #:alter-role-set-search-path
   #:flush-prepared-queries)

  ;; Model creation and management interface
  (:export
   #:create-model
   #:model-exists-p
   #:ensure-model
   #:all-models
   #:drop-model
   #:*sequence*
   #:*key*
   #:*key-type*
   #:make-model-parameters)

  ;; Model interface
  (:export
   #:insert
   #:update
   #:fetch
   #:fetch-all
   #:excise
   #:excise-all
   #:keys
   #:tally
   #:filter
   #:exists
   #:distinct
   #:history)

  ;; Model/database interaction
  (:export
   #:with-model-transaction)

  ;; User queries and JSON syntactic sugar for S-SQL
  (:export
   #:define-json-query
   #:jdoc
   #:j->
   #:j->>
   #:to-jsonb
   #:jbuild)

  ;; Trival helper functions
  (:export
   #:stash-key
   #:stash-key-destructive
   #:obj
   #:pp-json)

  (:documentation "Postgres-JSON is a JSON document store for Common
Lisp using PostgreSQL"))
