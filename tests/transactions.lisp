;;;; These aren't real tests, it's just running the functions and
;;;; inspecting the log output to see what the transaction code is
;;;; doing.

(defpackage :tran-test
  (:use :cl :postgres-json :postgres-json-model)
  (:shadowing-import-from :postgres-json :get :delete :count))

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
      (let ((key (insert 'pgj-tran-1 1)))
        (insert 'pgj-tran-2 2)
        (handler-bind ((database-safety-net #'really-do-it))
          (drop-model! 'pgj-tran-3))
        (delete 'pgj-tran-1 key)))))

(defun test-4 ()
  (with-conn ()
    (with-model-transaction (foo)
      (let ((bill-key (insert 'pgj-tran-1 "Bill")))
        (log:info "bill-key ~A" bill-key)
        (with-model-transaction (bar)
          (let ((mary-key (insert 'pgj-tran-1 "Mary")))
            (log:info "mary-key: ~A" mary-key)
            (update 'pgj-tran-1 mary-key "Maria")
            (get 'pgj-tran-1 mary-key)))
        (get 'pgj-tran-1 bill-key)))))

(defun test-5 ()
  (with-conn ()
    (with-model-transaction (foo)
      (let ((client-key (insert 'pgj-tran-1 "Gormless Guttering")))
        (let ((invoice-key (insert 'pgj-tran-2 (obj "client-key" client-key
                                                   "number" "123"))))
          (pp-json (get 'pgj-tran-2 invoice-key)))))))
