(defpackage :postgres-json-test
  (:use #:cl #:postgres-json #:1am #:alexandria)
  (:import-from :postgres-json #:geth #:len)
  (:export #:run-pgj-tests)
  (:documentation "Test Postgres-JSON."))
