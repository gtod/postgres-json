(defpackage :postgres-json
  (:use #:cl #:alexandria #:postmodern #:s-sql)
  (:shadow #:sequence)

  ;; Postgres backend
  (:export
   #:database-safety-net
   #:really-do-it
   #:create-backend
   #:backend-exists-p
   #:ensure-backend
   #:drop-backend)

  ;; Model creation and management
  (:export
   #:create-model
   #:model-exists-p
   #:ensure-model
   #:drop-model
   #:all-models)

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

  ;; Model and database interaction
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

  ;; Model parameters
  (:export
   #:*sequence*
   #:*key*
   #:*key-type*
   #:*gin-operator-class*
   #:make-model-parameters)

  ;; Trival helper functions
  (:export
   #:stash-key
   #:stash-key-destructive
   #:obj
   #:pp-json)

  ;; Miscellaneous backend functions
  (:export
   #:create-db-sequence
   #:alter-role-set-search-path
   #:flush-prepared-queries)

  ;; Specials
  (:export
   #:*pgj-schema*
   #:*default-search-path*
   #:*to-json*
   #:*from-json*
   #:*stash-key*
   #:*db-handle-serialization-failure-p*
   #:*serialization-failure-sleep-times*)

  (:documentation "Postgres-JSON is a JSON document store for Common
Lisp using PostgreSQL"))
