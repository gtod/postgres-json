(in-package :pj-human)

;;; Set the *postmodern-connection* form below for your Postgres 9.4
;;; DB if you have not done so elsewhere. Then do (create),
;;; (load-humans) and (model-test)

;;; See the User Guide for further details

;;; Examples of user defined queries are in human-2.lisp, you might
;;; want to try them before running the cleanup or drop forms.

;; Set this if you have not already done so
;; (setf *postmodern-connection* '("mydb" "myusername" "" "localhost"))

(defparameter *human-url* "http://gtod.github.io/human.json")
(defparameter *human-file* "/tmp/postgres-json-human.json")

;;;; Backend interface

(defun create ()
  (with-pj-connection ()
    (ensure-backend)
    (ensure-model 'human)
    (ensure-model 'gift)))

(defun cleanup ()
  (with-pj-connection()
    (excise-all 'human)
    (excise-all 'gift)))

(defun drop ()
  (with-pj-connection ()
    (drop-model 'human)
    (drop-model 'gift)))

;;;; Human model

(defun random-human ()
  (fetch 'human (elt (keys 'human) (random (tally 'human)))))

(defun load-humans ()
  (unless (probe-file *human-file*)
    (write-line "Fetching humans...")
    (ql-http:fetch *human-url* *human-file*))

  (with-input-from-file (stream *human-file*)
    (with-pj-connection ()
      (with-model-transaction ()
        (write-line "Loading humans...")
        (loop for human across (yason:parse stream :json-arrays-as-vectors t)
              do (insert 'human human)
              finally (return (tally 'human)))))))

;;;; Interface

(defun model-test ()
  (with-pj-connection ()
    (show (length (filter 'human :contain (obj "gender" "female"))))

    (show (gethash "name" (random-human)))

    (let ((human (show (first (filter 'human :contain (obj "name" "Marcella Marquez"))))))
      (with-keys ((key "key") (friends "friends")) human
        (push (obj "name" "Horace Morris" "id" (length friends)) friends)
        (show (update 'human key human))
        (show (gethash "friends" (fetch 'human key)))
        (show (history 'human key))))

    (show (filter 'human :properties '("age" "tags") :contain (obj "tags" '("ut" "labore"))))
    (show (length (filter 'human :contain (obj "isActive" t "age" 21))))

    (show (length (exists 'human "eyeColor")))
    (show (distinct 'human "favoriteFruit")))
  (values))
