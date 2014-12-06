(defpackage :postgres-json
  (:use #:cl #:alexandria #:postmodern #:s-sql)
  (:shadow #:get #:delete #:count #:sequence)

  ;; Specials
  (:export
   #:*pgj-schema*
   #:*default-search-path*
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
   #:create-db-sequence
   #:create-backend
   #:backend-exists-p
   #:alter-role-set-search-path
   #:create-model
   #:model-exists-p
   #:all-models
   #:drop-backend!
   #:drop-model!
   #:define-json-query
   #:flush-prepared-queries
   #:make-model-parameters)

  ;; Trival helper functions
  (:export
   #:stash-key
   #:stash-key-destructive
   #:obj
   #:pp-json))
