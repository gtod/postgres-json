(defpackage :pj-test
  (:nicknames :pj-test)
  (:use :cl :postmodern :postgres-json)
  (:import-from :postgres-json :obj :pp-json))

(in-package :pj-test)

;;; See quickstart to ensure you have created the default schema,
;;; sequence and the backend for cat

(eval-when (:compile-toplevel :load-toplevel :execute)
  (bake-interface cat))

(defun insert-some (&optional (number 40))
  (dotimes (i number)
    (cat:insert (obj "name" (format nil "name-~A" i) "coat" "scruffy"))))

;; quickload bordeaux-threads if need be

(log:config :debug)

(defun update-some ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-connection '("cusoon" "gtod" "" "localhost" :port 5433)
                (cat:update id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
    (loop for id from 4 to 23
          do (update-cat id))))

;; In PSQL: set search_path pgj_schema,public;
;; now try select count(*) from cat_old;

(defun update-one ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-connection '("cusoon" "gtod" "" "localhost" :port 5433)
                (cat:update id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
    (dotimes (i 20)
      (update-cat 1))))

;; now try select count(*) from cat_old;

(defun update-one-allow-failure ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-connection '("cusoon" "gtod" "" "localhost" :port 5433)
                (let ((*db-handle-serialization-failure-p* nil))
                  (cat:update id (obj "name" (format nil "name-~A" id) "coat" "scruffy"))))))))
    (dotimes (i 3)
      (update-cat 1))))
