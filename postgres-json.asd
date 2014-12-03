(asdf:defsystem postgres-json
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.5"
  :license "MIT"
  :homepage "https://github.com/gtod/postgres-json"
  :description "Create trivial, effectively immutable, simple Common
Lisp object persistence models using the PostgreSQL 9.4+ jsonb type."
  :depends-on (#:alexandria
               #:postmodern
               #:bordeaux-threads
               #:log4cl
               #:closer-mop
               #:yason)
  :serial t
  :components
  ((:file "package")
   (:file "util")
   (:file "errors")
   (:file "specials")
   (:file "postgres/s-sql")
   (:file "postgres/util")
   (:file "postgres/commands")
   (:file "postgres/transactions")
   (:file "model/backend")
   (:file "model/parameters")
   (:file "model/query")
   (:file "model/interface")
   (:file "model/package")
   (:file "interface")))
