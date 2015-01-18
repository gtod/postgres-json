(defpackage :postgres-json
  (:use #:cl #:alexandria #:postmodern #:s-sql #:global-vars)

  ;; Connections
  (:export
   #:*postmodern-connection*
   #:ensure-top-level-connection)

  ;; Model types
  (:export
   #:pgj-model
   #:pgj-history-model
   #:pgj-object-model
   #:pgj-history-object-model)

  ;; Basic model management
  (:export
   #:define-global-model
   #:ensure-backend
   #:drop-backend)

  ;; Model CRUD generic functions
  (:export
   #:insert
   #:supersede
   #:fetch
   #:fetch-all
   #:excise
   #:excise-all
   #:keys
   #:tally
   #:having-property
   #:enumerate-property
   #:filter
   #:history)

  ;; Model transactions
  (:export
   #:with-model-transaction
   #:rollback
   #:commit
   #:*serialization-failure-sleep-times*)

  ;; JSON helper functions and specials
  (:export
   #:obj
   #:pp-json
   #:*to-json*
   #:*from-json*)

  ;; User queries and JSON syntactic sugar for S-SQL
  (:export
   #:define-json-query
   #:jdoc
   #:j->
   #:j->>
   #:to-jsonb
   #:jbuild)

  ;; Model customization generic functions
  (:export
   #:model-sequence
   #:model-key-name
   #:model-key-type
   #:model-initial-gin-operator-class
   #:serialize
   #:deserialize
   #:stash)

  ;; Further model management
  (:export
   #:create-backend
   #:backend-exists-p
   #:database-safety-net
   #:really-do-it
   #:*gin-operator-classes*
   #:use-gin-index)

  ;; Postgres backend
  (:export
   #:*pgj-schema*
   #:drop-pgj-schema
   #:*default-search-path*
   #:alter-role-set-search-path
   #:create-db-sequence
   #:flush-prepared-queries)

  ;; Postmodern isolation level transactions
  (:export
   #:*pgj-default-isolation-level*
   #:incompatible-transaction-setting

   #:serializable-rw
   #:repeatable-read-rw
   #:read-committed-rw
   #:read-committed-ro

   #:with-transaction-level
   #:with-logical-transaction-level
   #:ensure-transaction-level)

  ;; lparallel support (optional)
  (:export
   #:*pgj-kernel*
   #:*pgj-database*
   #:make-pgj-kernel
   #:end-pgj-kernel

   #:call-with-connected-thread
   #:with-connected-thread

   #:*pgj-channel*
   #:make-pgj-channel

   #:submit-pgj-function
   #:submit-pgj-task
   #:receive-pgj-result
   #:try-receive-pgj-result)

  (:documentation "Postgres-JSON is a JSON document store for Common
Lisp using PostgreSQL"))
