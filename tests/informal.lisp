;;;; Some informal tests for the REPL
;;;; Compile this file and, create-test-moldes, call some functions
;;;; and finally call drop-test-models to clean up.

(defpackage :pj-test
  (:use :cl :postgres-json)
  (:shadowing-import-from :postgres-json :get :delete :count)
  (:import-from :postgres-json :obj :pp-json))

(in-package :pj-test)

(defparameter *bookings-json-file*
  (merge-pathnames "tests/bookings.json" (asdf:system-source-directory :postgres-json)))

(defparameter *connection* '("cusoon" "gtod" "" "localhost" :port 5433 :pooled-p t))

(defmacro with-conn (() &body body)
  `(pomo:with-connection *connection*
     ,@body))

;; Run first
(defun create-test-models ()
  (log:config :debug)
  (with-conn ()
    (unless (backend-exists-p)
      (create-backend))
    (create-model 'pgj-booking)
    (create-model 'pgj-cat)))

;; Run last
(defun drop-test-models ()
  (flet ((extreme-prejudice (c)
           (declare (ignore c))
           (invoke-restart 'really-do-it)))
    (with-conn ()
      (handler-bind ((database-safety-net #'extreme-prejudice))
        (drop-model! 'pgj-booking)
        (drop-model! 'pgj-cat))))
  (log:config :info))

(defun insert-bookings ()
  (with-conn ()
    (alexandria:with-input-from-file (stream *bookings-json-file*)
      (dolist (booking (yason:parse stream))
        (insert 'pgj-booking booking)))))

(defun insert-some-cats (&optional (number 40))
  (with-conn ()
    (dotimes (i number)
      (insert 'pgj-cat (obj "name" (format nil "name-~A" i) "coat" "scruffy")))))

#+bordeaux-threads
(defun update-some ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-conn ()
                (update 'pgj-cat id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
    (with-conn ()
      (dolist (id (keys 'pgj-cat))
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
              (with-conn ()
                (update 'pgj-cat id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
    (with-conn ()
      (dotimes (i 20)
        (update-cat (first (keys 'pgj-cat)))))))

#+bordeaux-threads
(defun update-one-allow-failure ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-conn ()
                (let ((*db-handle-serialization-failure-p* nil))
                  (update 'pgj-cat id (obj "name" (format nil "name-~A" id) "coat" "scruffy"))))))))
    (with-conn ()
      (dotimes (i 3)
        (update-cat (first (keys 'pgj-cat)))))))
