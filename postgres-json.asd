(asdf:defsystem postgres-json
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.5"
  :license "MIT"
  :homepage "https://github.com/gtod/postgres-json"
  :description "Create trivial, effectively immutable, simple Common
Lisp object persistence models using the PostgreSQL 9.4+ jsonb type."
  :depends-on (#:alexandria
               #:postmodern
               #:log4cl
               #:yason)
  :components
  ((:file "package")
   (:file "util")
   (:file "errors")
   (:file "postgres")
   (:file "transactions")
   (:file "model")
   (:file "interface")))
