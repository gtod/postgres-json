(defpackage :conn-test
  (:use :cl :postgres-json :postgres-json-model :alexandria)
  (:shadowing-import-from :postgres-json :get :delete :count)
  (:import-from :postgres-json :symbol->json))

(in-package :conn-test)

(defparameter *connection* '("cusoon" "gtod" "" "localhost" :port 5433 :pooled-p t))

(defmacro with-conn (() &body body)
  `(pomo:with-connection *connection*
     ,@body))

(defun run (total-threads)
  (setup)
  (log:config :debug)
  (dolist (thread (launch-threads total-threads))
    (bt:join-thread thread))
  (review))

(defun setup ()
  (with-conn ()
    (with-model-transaction ()
      (unless (model-exists-p 'thread-test)
        (create-model 'thread-test)))))

(defun cleanup ()
  (with-conn ()
    (delete-all 'thread-test)))

(defun pomo-meta-slot (db)
  (let ((meta 'cl-postgres::meta))
    (if (and (slot-boundp db meta) (hash-table-p (slot-value db meta)))
        (mapcar #'symbol->json (alexandria:hash-table-keys (slot-value db meta)))
        nil)))

(defun launch-thread (name)
  (bt:make-thread
   (lambda ()
     (with-conn ()
       (let* ((db pomo:*database*)
              (obj (let ()
                     (obj "stamp" (get-internal-real-time)
                          "thread" (bt:thread-name (bt:current-thread))
                          "connection" (obj "object" (format nil "~A" db)
                                            "meta" (pomo-meta-slot db))))))
         (insert 'thread-test obj :stash-key 'stash-key))))
   :name name))

(defun launch-threads (number)
  (let ((threads '()))
    (dotimes (i number threads)
      (push (launch-thread (format nil "~A" (1+ i))) threads)
      ;(sleep (/ (random 20) 1000))
      )))

(defun review ()
  (flet ((prop (key)
           (lambda (obj)
             (gethash key obj))))
    ;; (pomo:query "select jdoc from pgj_model.thread_test order by jdoc->'stamp'")
    ;; This above is what we need to support sorting in the DB...
    (with-conn ()
      (pp-json (sort (all 'thread-test) #'< :key (prop "stamp"))))))
