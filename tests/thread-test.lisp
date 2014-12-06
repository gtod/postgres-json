;;;; Some informal thread tests for the REPL
;;;; Compile this file, call create-test-model, call some functions
;;;; and finally call drop-test-model to clean up.

;;;; Safe to run as long as you don't have REAL data in a 'pj-cat
;;;; model (which would be unlikely).

(defpackage :pj-test
  (:use :cl :postgres-json))

(in-package :pj-test)

(log:config :debug)

(defparameter *connection* '("cusoon" "gtod" "" "localhost" :port 5433 :pooled-p t))

;;;; Implementation

(defmacro handle-cl-postgres-error (() &body body)
  `(handler-case
       (progn ,@body)
     (cl-postgres-error:insufficient-resources (e)
       (log:debug "Handle insufficient resources: ~A" e))
     (cl-postgres:database-error (e)
       (log:debug "cl-postgres db error: ~A" e))))

(defmacro with-conn (() &body body)
  `(handle-cl-postgres-error ()
     (pomo:with-connection *connection*
       ,@body)))

(defun update-cat (key)
  (bt:make-thread
   (lambda ()
     (with-conn ()
       (pj:update 'pj-cat key (obj "name" (format nil "name-~A" key) "coat" "scruffy"))))))

(defun really-do-it (c)
  (declare (ignore c))
  (invoke-restart 'really-do-it))

;;;; Interface

;; Run first
(defun create-test-model ()
  (with-conn ()
    (unless (backend-exists-p)
      (create-backend))
    (create-model 'pj-cat)))

;; Run second
(defun insert-some-cats (&optional (number 40))
  (with-conn ()
    ;; Transactions are optional, but for bulk inserts they make just
    ;; one transaction rather than 40 or whatever...
    (with-model-transaction (some-cats)
      (dotimes (i number)
        (pj:insert 'pj-cat (obj "name" (format nil "name-~A" i) "coat" "scruffy"))))))

;; Run third
(defun update-some ()
  (with-conn ()
    (mapc #'bt:join-thread
          (loop for key in (pj:keys 'pj-cat)
                collect (update-cat key)))))

;; In production code you would certainly not expect to see 19
;; different users all trying to update a single record at once.  But
;; this is an interesting, completely informal, stress test of the
;; serialization handling code.

;; (query (sql-compile `(:select (:count '*) :from (:dot ,*test-schema* 'cat-old))) :single)
;; Why is this not an interface function?
;; In PSQL: set search_path to pj_model,public;
;; and try select count(*) from cat_old;

;; Run fourth
(defun update-one ()
  (with-conn ()
    (mapc #'bt:join-thread
          (loop with key = (first (pj:keys 'pj-cat))
                for i from 1 to 20
                collect (update-cat key)))))

;; Run last
(defun drop-test-model ()
  (with-conn ()
    (handler-bind ((database-safety-net #'really-do-it))
      (drop-model! 'pj-cat)))
  (log:config :info))
