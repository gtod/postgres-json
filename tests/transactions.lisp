;;;; These aren't real tests, it's just running the functions and
;;;; inspecting the log output to see what the transaction code is
;;;; doing.

(defpackage :tran-test
  (:use :cl :postgres-json)
  (:shadowing-import-from :postgres-json :get :delete :count)
  (:import-from :postgres-json :obj :pp-json))

(in-package :tran-test)

(defparameter *connection* '("cusoon" "gtod" "" "localhost" :port 5433 :pooled-p t))

(defmacro with-conn (() &body body)
  `(pomo:with-connection *connection*
     ,@body))

(defun create-tran-test ()
  (log:config :trace)
  (with-conn ()
    (unless (backend-exists-p)
      (create-backend))
    (create-model 'pgj-tran-1)
    (create-model 'pgj-tran-2)))

(defun really-do-it (c)
  (declare (ignore c))
  (invoke-restart 'really-do-it))

(defun drop-tran-test ()
  (with-conn ()
    (handler-bind ((database-safety-net #'really-do-it))
      (drop-model! 'pgj-tran-1)
      (drop-model! 'pgj-tran-2)))
  (log:config :info))

(defun test-1 ()
  (with-conn ()
    (insert 'pgj-tran-1 1)
    (insert 'pgj-tran-2 2)))

(defun test-2 ()
  (with-conn ()
    (with-model-transaction (foo)
      (insert 'pgj-tran-1 1)
      (insert 'pgj-tran-2 2))))

(defun test-3 ()
  (with-conn ()
    (with-model-transaction (foo)
      (create-model 'pgj-tran-3)
      (let ((id (insert 'pgj-tran-1 1)))
        (insert 'pgj-tran-2 2)
        (handler-bind ((database-safety-net #'really-do-it))
          (drop-model! 'pgj-tran-3))
        (delete 'pgj-tran-1 id)))))

(defun test-4 ()
  (with-conn ()
    (with-model-transaction (foo)
      (let ((bill-id (insert 'pgj-tran-1 "Bill")))
        (log:info "bill-id ~A" bill-id)
        (with-model-transaction (bar)
          (let ((mary-id (insert 'pgj-tran-1 "Mary")))
            (log:info "mary-id: ~A" mary-id)
            (update 'pgj-tran-1 mary-id "Maria")
            (get 'pgj-tran-1 mary-id)))
        (get 'pgj-tran-1 bill-id)))))

(defun test-5 ()
  (with-conn ()
    (with-model-transaction (foo)
      (let ((client-id (insert 'pgj-tran-1 "Gormless Guttering")))
        (let ((invoice-id (insert 'pgj-tran-2 (obj "client-id" client-id
                                                   "number" "123"))))
          (pp-json (get 'pgj-tran-2 invoice-id)))))))
