;;;; Some informal tests for the REPL

(defpackage :pj-test
  (:use :cl :postgres-json :postmodern)
  (:shadowing-import-from :postgres-json :get :delete)
  (:import-from :postgres-json :obj :pp-json))

(in-package :pj-test)

(defparameter *bookings-json-file*
  (merge-pathnames "tests/bookings.json" (asdf:system-source-directory :postgres-json)))

(defparameter *connection* '("cusoon" "gtod" "" "localhost" :port 5433 :pooled-p t))

(defparameter *test-schema* 'gtod-net-postgresql-json-test-schema)

(defmacro with-schema-connection (() &body body)
  `(with-db-schema (*test-schema*)
     (with-connection *connection*
       ,@body)))

(defun setup ()
  (log:config :debug)
  (with-db-schema (*test-schema*)
    (create-backend)
    (create-model 'booking)
    (create-model 'cat)))

(defun insert-bookings ()
  (with-db-schema (*test-schema*)
    (alexandria:with-input-from-file (stream *bookings-json-file*)
      (dolist (booking (yason:parse stream))
        (insert 'booking booking)))))

(defun insert-some-cats (&optional (number 40))
  (with-db-schema (*test-schema*)
    (dotimes (i number)
      (insert 'cat (obj "name" (format nil "name-~A" i) "coat" "scruffy")))))

(defun drop-test-schema ()
  (with-db-schema (*test-schema*)
    (drop-backend!)))

#+bordeaux-threads
(defun update-some ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-schema-connection ()
                (update 'cat id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
    (loop for id from 4 to 23
          do (update-cat id))))

;; In PSQL: set search_path gtod_net_postgresql_json_test_schema,public;
;; now try select count(*) from cat_old;

#+bordeaux-threads
(defun update-one ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-schema-connection ()
                (update 'cat id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
    (dotimes (i 20)
      (update-cat 1))))

#+bordeaux-threads
(defun update-one-allow-failure ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-schema-connection ()
                (let ((*db-handle-serialization-failure-p* nil))
                  (update 'cat id (obj "name" (format nil "name-~A" id) "coat" "scruffy"))))))))
    (dotimes (i 3)
      (update-cat 1))))
