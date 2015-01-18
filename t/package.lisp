(defpackage :postgres-json-test
  (:use #:cl #:postgres-json #:1am #:alexandria)
  (:export #:run-pgj-tests)
  (:documentation "Test Postgres-JSON."))
