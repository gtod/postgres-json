;;;; At one stage there was a *model-parameters* hash to cache the
;;;; model params once they had been fetch from the *meta-model*
;;;; table.  Because this hash would have been accessed by all threads
;;;; it needed a lock for write access.  The following code was a
;;;; demonstration of that need (thanks to the SBCL doc on threads).

;;;; However, it was simpler to just stop caching the model parameters
;;;; because it amounted to a premature optimization: because the
;;;; query gets prepared and put into *query-functions* it's not as if
;;;; we have to go to the DB for model params for each model request...

;;;; So this is just a reminder of the need for thread safe locks on
;;;; hash table writes.

(defpackage :thread-lock-test
  (:use :cl)
  (:import-from :postgres-json :pp-json))

(in-package :thread-lock-test)

(defparameter *trample* (make-hash-table :test #'equal))

(defun run (total-threads)
  (setf *trample* (make-hash-table :test #'equal))
  (mapc #'bt:join-thread
        (loop for i from 1 to total-threads
              collect (bt:make-thread
                       (lambda ()
                         (loop repeat 1000
                               do (incf (gethash "foo" *trample* 0))
                               (sleep 0.00001))))))
  (pp-json *trample*))

(defun trample ()
  (setf *trample* (make-hash-table :test #'equal))
  (let ((lock (bt:make-lock)))
    (mapc #'bt:join-thread
          (loop for i from 1 to 1000
                collect (bt:make-thread
                         (lambda ()
                           (loop repeat 1000
                                 do (bt:with-lock-held (lock)
                                      (incf (gethash "foo" *trample* 0)))
                                    (sleep 0.00001)))))))
  (pp-json *trample*))
