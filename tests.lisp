(defpackage :pj-test
  (:use :cl :postgres-json)
  (:import-from :postgres-json :obj :pp-json))

(in-package :pj-test)

(defvar *test-schema* 'net-gtod-postgres-json-test-schema)

(defparameter *test-schema-exists-message*
  "Not willing to run tests in existing schema.  Unless this schema
contains important data of yours (unlikey given the name its got but
you can never be too careful with other people's data), you might want
to drop it with (drop-db-schema-cascade *test-schema*) and try
again.~%Schema: ~A")

(defparameter *bookings-json* 
  (merge-pathnames "tests.json" (asdf:system-source-directory :postgres-json)))

;; Do we just get around this by putting it in a separate file we
;; compile before this file?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-model booking))

(defmacro bake ()
  `(bake-model booking :schema ,*test-schema*))

(defun setup ()
  (if (pomo:schema-exist-p *test-schema*)
      (error *test-schema-exists-message* *test-schema*)
      (let ((*db-schema* *test-schema*))
        (create-db-schema)
        (create-db-sequence)
        (create-model-backend 'booking)
        (bake))))

(defun insert-bookings ()
  (alexandria:with-input-from-file (stream *bookings-json*)
    (dolist (booking (yason:parse stream))
      (booking:insert booking))))

;; (defun insert-some (&optional (number 40))
;;   (dotimes (i number)
;;     (cat:insert (obj "name" (format nil "name-~A" i) "coat" "scruffy"))))

;; ;; quickload bordeaux-threads if need be

;; (log:config :debug)

;; (defun update-some ()
;;   (flet ((update-cat (id)
;;            (bt:make-thread
;;             (lambda ()
;;               (with-connection '("cusoon" "gtod" "" "localhost" :port 5433)
;;                 (cat:update id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
;;     (loop for id from 4 to 23
;;           do (update-cat id))))

;; ;; In PSQL: set search_path pgj_schema,public;
;; ;; now try select count(*) from cat_old;

;; (defun update-one ()
;;   (flet ((update-cat (id)
;;            (bt:make-thread
;;             (lambda ()
;;               (with-connection '("cusoon" "gtod" "" "localhost" :port 5433)
;;                 (cat:update id (obj "name" (format nil "name-~A" id) "coat" "scruffy")))))))
;;     (dotimes (i 20)
;;       (update-cat 1))))

;; ;; now try select count(*) from cat_old;

;; (defun update-one-allow-failure ()
;;   (flet ((update-cat (id)
;;            (bt:make-thread
;;             (lambda ()
;;               (with-connection '("cusoon" "gtod" "" "localhost" :port 5433)
;;                 (let ((*db-handle-serialization-failure-p* nil))
;;                   (cat:update id (obj "name" (format nil "name-~A" id) "coat" "scruffy"))))))))
;;     (dotimes (i 3)
;;       (update-cat 1))))
