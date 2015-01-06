(asdf:defsystem postgres-json-test
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.5"
  :license "MIT"
  :homepage "https://github.com/gtod/postgres-json"
  :description "Tests for Postgres-JSON, a Postgres JSON document store"
  :depends-on (#:postgres-json-parallel #:1am)
  :components
  ((:module "t"
    :serial t
    :components ((:file "package")
                 (:file "base")
                 (:file "transactions")
                 (:file "model/interface")))))
