;;;; Some informal tests for the REPL

;;; Note that (in my imagination at least) there is no need for normal
;;; user code to use a with-db-schema form since all user model tables
;;; are created in *pgj-schema* which defaults to 'pgj_model and
;;; everyone is happy.  For these tests I just wanted to make a new
;;; schema, run the tests and then blow the schema away...

(defpackage :pj-test
  (:use :cl :postgres-json :postmodern)
  (:shadowing-import-from :postgres-json :get :delete)
  (:import-from :postgres-json :obj :pp-json))

(in-package :pj-test)

(defparameter *bookings-json-file*
  (merge-pathnames "tests/bookings.json" (asdf:system-source-directory :postgres-json)))

(defparameter *connection* '("cusoon" "gtod" "" "localhost" :port 5433 :pooled-p t))

(defparameter *test-schema* 'gtod-net-postgresql-json-test-schema)

(log:config :debug)

(defmacro with-schema-connection (() &body body)
  `(with-db-schema (*test-schema*)
     (with-connection *connection*
       ,@body)))

;; Run first
(defun setup ()
  (log:config :debug)
  (with-db-schema (*test-schema*)
    (create-backend)
    (create-model 'booking)
    (create-model 'cat)))

;; Run last
(defun drop-test-schema ()
  (with-db-schema (*test-schema*)
    (drop-backend!)))

(defun insert-bookings ()
  (with-db-schema (*test-schema*)
    (alexandria:with-input-from-file (stream *bookings-json-file*)
      (dolist (booking (yason:parse stream))
        (insert 'booking booking)))))

(defun insert-some-cats (&optional (number 40))
  (with-db-schema (*test-schema*)
    (dotimes (i number)
      (insert 'cat (obj "name" (format nil "name-~A" i) "coat" "scruffy")))))

#+bordeaux-threads
(defun update-some ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-schema-connection ()
                (update 'cat id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
    (with-schema-connection ()
      (dolist (id (keys 'cat))
        (update-cat id)))))

;; In production code you would certainly not expect to see 19
;; different users all trying to update a single record at once.  But
;; this is an interesting, completely informal, stress test of the
;; serialization handling code.

;; (query (sql-compile `(:select (:count '*) :from (:dot ,*test-schema* 'cat-old))) :single)
;; Why is this not an interface function?
;; In PSQL: set search_path gtod_net_postgresql_json_test_schema,public;
;; and try select count(*) from cat_old;
#+bordeaux-threads
(defun update-one ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-schema-connection ()
                (update 'cat id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
    (with-schema-connection ()
      (dotimes (i 20)
        (update-cat (first (keys 'cat)))))))

#+bordeaux-threads
(defun update-one-allow-failure ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-schema-connection ()
                (let ((*db-handle-serialization-failure-p* nil))
                  (update 'cat id (obj "name" (format nil "name-~A" id) "coat" "scruffy"))))))))
    (with-schema-connection ()
      (dotimes (i 3)
        (update-cat (first (keys 'cat)))))))
