(defpackage :postgres-json-transaction-test
  (:use #:cl #:postgres-json #:1am)
  (:export #:setup #:run #:teardown)
  (:documentation "Test Postgres-JSON."))
