;;;; At present this is only used for the Postgres timestamps
;;;; associated with the HISTORY of an object.  It is not much use to
;;;; times inside JSON objects because they have no Postgres types for
;;;; :cl-postgres+local-time to get a grip on.

(asdf:defsystem postgres-json-time
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.1.5"
  :license "MIT"
  :homepage "https://github.com/gtod/postgres-json"
  :description "cl-rfc3339 support for Postgres-JSON, a Postgres JSON document store"
  :depends-on (#:postgres-json
               #:local-time
               #:cl-postgres+local-time
               #:cl-rfc3339)
  :components
  ((:module "postgres"
    :serial t
    :components ((:file "cl-rfc3339")))))
