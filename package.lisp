(defpackage :postgres-json
  (:use #:cl #:alexandria #:postmodern #:s-sql)
  (:shadow #:sequence)

  ;; Postgres backend
  (:export
   #:*postmodern-connection*
   #:database-safety-net
   #:really-do-it
   #:ensure-top-level-connection
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

  ;; User transaction handling
  (:export
   #:*serialization-failure-sleep-times*
   #:with-model-transaction
   #:rollback
   #:commit)

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

  ;; lparallel support (optional)
  (:export
   #:*pgj-kernel*
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

  ;; Specials
  (:export
   #:*postmodern-connection*
   #:*pgj-schema*
   #:*default-search-path*
   #:*to-json*
   #:*from-json*
   #:*stash-key*)

  (:documentation "Postgres-JSON is a JSON document store for Common
Lisp using PostgreSQL"))
