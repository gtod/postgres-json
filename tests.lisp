(defpackage :pj-test
  (:nicknames :pj-test)
  (:use :cl :postgres-json)
  (:import-from :postmodern :connect-toplevel :with-connection))

(in-package :pj-test)

(defun hash-list (&rest args)
  "Return an 'equal hash-table consisting of pairs of ARGS."
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (key val) on args by #'cddr do
      (setf (gethash key hash) val))
    hash))

;;; In PSQL: set search_path pgj_schema,public;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (bake-interface cat))

(defun insert-some (number)
  (dotimes (i number)
    (cat:insert (hash-list "name" (format nil "name-~A" i) "coat" "scruffy"))))

(defun update-some ()
  (flet ((update-cat (id)
           (bt:make-thread
            (lambda ()
              (with-connection '("cusoon" "gtod" "" "localhost" :port 5433)
                (cat:update id (hash-list "name" (format nil "name-~A" id) "coat" "scruffy")))))))
    (loop for id from 4 to 23
          do (update-cat id))))
