;;; My own preference is to import :postgres-json-model as well, but
;;; the model functions appear here qualified by their nickname, such
;;; as pj:get, for clarity.

(defpackage :human
  (:use :cl :postgres-json :alexandria))

(in-package :human)

;; Start with these two if necessary
;; (pomo:connect-toplevel "cusoon" "gtod" "" "localhost" :port 5433)
;; (create-backend)

(defparameter *human-url* "http://gtod.github.io/human.json")
(defparameter *human-file* "/tmp/postgres-json-human.json")

(defun create ()
  (create-model 'human)
  (unless (probe-file *human-file*)
    (ql-http:fetch *human-url* *human-file*))
  (with-input-from-file (stream *human-file*)
    (with-model-transaction ()
      (dolist (human (yason:parse stream) (pj:count 'human))
        (pj:insert 'human human)))))

(defun drop ()
  (drop-model! 'human))

(defmacro show (form)
  `(progn
     (print ',form)
     (pp-json ,form)))

(defun run ()
  (show (length (pj:filter 'human :contain (obj "gender" "female"))))

  (let ((keys (pj:keys 'human)))
    (let ((id (elt keys (random (pj:count 'human)))))
      (show (gethash "name" (pj:get 'human id)))))

  (let ((human (show (first (pj:filter 'human :contain (obj "name" "Marcella Marquez"))))))
    (symbol-macrolet ((key (gethash "key" human))
                      (friends (gethash "friends" human)))
      (let* ((next-id (1+ (gethash "id" (last-elt friends))))
             (new-friend (obj "id" next-id "name" "Horace Morris")))
        (setf friends (reverse (cons new-friend (reverse friends))))
        (show (pj:update 'human key human))
        (show (length (gethash "friends" (pj:get 'human key))))
        (show (length (pj:history 'human key)))))))
