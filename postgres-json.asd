(asdf:defsystem postgres-json
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.2.0"
  :license "MIT"
  :homepage "https://github.com/gtod/postgres-json"
  :description "Store and query JSON documents in PostgreSQL"
  :depends-on (#:alexandria
               #:postmodern
               #:global-vars
               #:log4cl
               #:yason)
  :serial t
  :components
  ((:module "base"
    :serial t
    :components ((:file "package")
                 (:file "util")
                 (:file "errors")
                 (:file "specials")))
   (:module "postgres"
    :serial t
    :components ((:file "s-sql")
                 (:file "postmodern")
                 (:file "util")
                 (:file "commands")
                 (:file "transactions")))
   (:module "model"
    :serial t
    :components ((:file "transactions")
                 (:file "user-query")
                 (:file "types")
                 (:file "model")
                 (:file "history")
                 (:file "query")
                 (:file "interface")))
   (:file "base/interface")))
