(defpackage :postgres-json
  (:use #:cl #:alexandria #:postmodern #:s-sql)
  (:shadow #:get #:delete #:count #:sequence)

  ;; Specials
  (:export
   #:*pgj-schema*
   #:*to-json*
   #:*from-json*
   #:*stash-key*
   #:*db-handle-serialization-failure-p*
   #:*serialization-failure-sleep-times*)

  ;; Setup a model
  (:export
   #:database-safety-net
   #:really-do-it
   #:with-model-transaction
   #:with-db-schema
   #:create-db-sequence
   #:create-backend
   #:backend-exists-p
   #:create-model
   #:model-exists-p
   #:all-models
   #:drop-backend!
   #:drop-model!
   #:flush-prepared-queries
   #:make-model-parameters)

  ;; Trival helper functions
  (:export
   #:stash-key
   #:stash-key-destructive
   #:obj
   #:pp-json))
