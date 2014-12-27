(asdf:defsystem postgres-json-parallel
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.5"
  :license "MIT"
  :homepage "https://github.com/gtod/postgres-json"
  :description "lparallel support for Postgres-JSON, a Postgres JSON document store"
  :depends-on (#:postgres-json
               #:lparallel)
  :components
  ((:file "parallel")))
