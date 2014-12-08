(in-package :pj-examples)

;;; Set the *connections* form below for your Postgres 9.4 DB and
;;; evaluate it. Then do (create), (load-humans) and (model-test)

;;; See the User Guide for further details

;;; Examples of user defined queries are in human-2.lisp, you might
;;; want to try them before running the cleanup or drop forms.

(defparameter *connection* '("cusoon" "gtod" "" "localhost" :port 5433))

(defparameter *human-url* "http://gtod.github.io/human.json")
(defparameter *human-file* "/tmp/postgres-json-human.json")

;;;; Backend interface

(defun create ()
  (ensure-backend)
  (ensure-model 'human)
  (ensure-model 'gift))

(defun cleanup ()
  (with-pj-connection()
    (pj:delete-all 'human)
    (pj:delete-all 'gift)))

(defun drop ()
  (with-pj-connection ()
    (drop-model! 'human)
    (drop-model! 'gift)))

;;;; Human model

(defun random-human ()
  (pj:get 'human (elt (pj:keys 'human) (random (pj:count 'human)))))

(defun load-humans ()
  (unless (probe-file *human-file*)
    (write-line "Fetching humans...")
    (ql-http:fetch *human-url* *human-file*))

  (with-input-from-file (stream *human-file*)
    (with-pj-connection ()
      (with-model-transaction ()
        (write-line "Loading humans...")
        (loop for human across (yason:parse stream :json-arrays-as-vectors t)
              do (pj:insert 'human human)
              finally (return (pj:count 'human)))))))

;;;; Interface

(defun model-test ()
  (with-pj-connection ()
    (show (length (pj:filter 'human :contain (obj "gender" "female"))))

    (show (gethash "name" (random-human)))

    (let ((human (show (first (pj:filter 'human :contain (obj "name" "Marcella Marquez"))))))
      (with-keys ((key "key") (friends "friends")) human
        (push (obj "name" "Horace Morris" "id" (length friends)) friends)
        (show (pj:update 'human key human))
        (show (gethash "friends" (pj:get 'human key)))
        (show (pj:history 'human key))))

    (show (pj:filter 'human :keys '("age" "tags") :contain (obj "tags" '("ut" "labore"))))
    (show (length (pj:filter 'human :contain (obj "isActive" t "age" 21))))

    (show (length (pj:exists 'human "eyeColor")))
    (show (pj:distinct 'human "favoriteFruit")))
  (values))
