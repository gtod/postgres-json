(asdf:defsystem postgres-json
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.0"
  :license "MIT"
  :description "Create a trivial, effectively immutable, simple Common
Lisp object persistence model using the PostgreSQL 9.4+ jsonb type."
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
   (:file "model-lib")
   (:file "model")
   (:file "interface")))
