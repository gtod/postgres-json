(asdf:defsystem postgres-json-fset
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.5"
  :license "MIT"
  :homepage "https://github.com/gtod/postgres-json"
  :description "Fset support for Postgres-JSON, a Postgres JSON document store"
  :depends-on (#:postgres-json
               #:jsown
               #:fset)
  :components
  ((:module "base"
    :serial t
    :components ((:file "fset")
                 (:file "yason")))))
