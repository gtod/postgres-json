(asdf:defsystem postgres-json-examples
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.5"
  :license "MIT"
  :homepage "https://github.com/gtod/postgres-json"
  :description "Examples for POSTGRES-JSON, a Postgres jsonb type
persistence library for CL."
  :depends-on (#:postgres-json
               #:alexandria
               #:postmodern
               #:yason)
  :components
  ((:module examples
    :serial t
    :components ((:file "package")
                 (:file "util")
                 (:file "simple-1")
                 (:file "simple-2")
                 (:file "human-1")
                 (:file "human-2")))))
