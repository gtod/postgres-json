(asdf:defsystem postgres-json-parallel
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.5"
  :license "MIT"
  :homepage "https://github.com/gtod/postgres-json"
  :description "lparallel support for Postgres-JSON, a Postgres JSON document store"
  :depends-on (#:postgres-json
               #:lparallel
               ;; Try git clone https://github.com/sionescu/bordeaux-threads.git
               ;; in your quicklips/local-projects and then (ql:register-local-projects)
               ;; at the REPL to fix this.
               (:version #:bordeaux-threads "0.8.3.99"))
  :components
  ((:module "base"
    :serial t
    :components ((:file "parallel")))))
