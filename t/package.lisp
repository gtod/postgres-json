(defpackage :postgres-json-test
  (:use #:cl #:postgres-json #:1am #:alexandria)
  (:export #:run-pgj-tests)
  (:documentation "Test Postgres-JSON."))

;;;; Instructions
;;; (ql:quickload :postgres-json-test)
;;; (in-package :postgres-json-test)
;;; (setf *postmodern-connection* '("mydb" "myuname" "" "mydbserver"))
;;; (run-pgj-tests)
