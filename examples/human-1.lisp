(in-package :pj-human)

;;; Set this if you have not already done so:
;;; (setf *postmodern-connection* '("mydb" "myusername" "" "localhost"))

;;; Then do (setup), (load-humans) and (model-test)

;;; Examples of user defined queries are in human-2.lisp, you might
;;; want to try them before running the cleanup or drop forms.

(defparameter *human-url* "http://gtod.github.io/human.json")
(defparameter *human-file* "/tmp/postgres-json-human.json")

(define-global-model human -human- (pgj-history-object-model))
(define-global-model gift -gift- (pgj-object-model))

;;;; Backend interface

(defun setup ()
  (with-pj-connection ()
    (ensure-backend -human-)
    (ensure-backend -gift-)))

(defun cleanup ()
  (with-pj-connection()
    (excise-all -human-)
    (excise-all -gift-)))

(defun drop ()
  (with-pj-connection ()
    (drop-backend -human-)
    (drop-backend -gift-)))

;;;; Human model

(defun random-human ()
  (let ((tally (tally -human-)))
    (assert (not (zerop tally)))
    (fetch -human- (elt (keys -human-) (random tally)))))

(defun load-humans ()
  (unless (probe-file *human-file*)
    (write-line "Fetching humans...")
    (ql-http:fetch *human-url* *human-file*))

  (with-input-from-file (stream *human-file*)
    (with-pj-connection ()
      (with-model-transaction ()
        (write-line "Loading humans...")
        (loop for human across (yason:parse stream :json-arrays-as-vectors t)
              do (insert -human- human)
              finally (return (tally -human-)))))))

;;;; Interface

(defun model-test ()
  (with-pj-connection ()
    (show (length (filter -human- :contains (obj "gender" "female"))))

    (show (gethash "name" (random-human)))

    (let ((human (show (first (filter -human- :contains (obj "name" "Marcella Marquez"))))))
      (with-keys ((key "key") (friends "friends")) human
        ;; This is naughty because it requires knowing yason vectors are adjustable.
        ;; But specialize DESERIALIZE on your model and do what you like...
        (vector-push-extend (obj "name" "Horace Morris" "id" (length friends)) friends)
        (show (supersede -human- key human))
        (show (gethash "friends" (fetch -human- key)))
        (show (history -human- key))))

    (show (filter -human- :contains (obj "tags" '("ut" "labore")) :properties '("age" "tags")))
    (show (length (filter -human- :contains (obj "isActive" t "age" 21))))

    (show (length (having-property -human- "eyeColor")))
    (show (enumerate-property -human- "favoriteFruit")))
  (values))
