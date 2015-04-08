(defpackage :pj-human
  (:use :cl :fset :gmap :postgres-json :alexandria)

  ;; postgres-json:insert beats out fset:insert
  (:shadowing-import-from :postgres-json :insert)
  ;; In general we prefer fset symbols to alexandria...
  (:shadowing-import-from :fset :unionf :appendf :removef)
  ;; but compose is too important to ignore.
  (:shadowing-import-from :alexandria :compose)

  (:shadowing-import-from :fset
			  ;; Shadowed type/constructor names
			  #:set #:map
			  ;; Shadowed set operations
			  #:union #:intersection #:set-difference #:complement
			  ;; Shadowed sequence operations
			  #:first #:last #:subseq #:reverse #:sort #:stable-sort
			  #:reduce
			  #:find #:find-if #:find-if-not
			  #:count #:count-if #:count-if-not
			  #:position #:position-if #:position-if-not
			  #:remove #:remove-if #:remove-if-not
			  #:substitute #:substitute-if #:substitute-if-not
			  #:some #:every #:notany #:notevery))

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

(fmakunbound 'show)
(defmacro show (form)
  `(progn
     (print ',form)
     (print ,form)))

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
    (fetch -human- (lookup (keys -human-) (random tally)))))

(defun load-humans ()
  (unless (probe-file *human-file*)
    (write-line "Fetching humans...")
    (ql-http:fetch *human-url* *human-file*))

  (with-pj-connection ()
    (with-model-transaction ()
      (write-line "Loading humans...")
      (let ((json (read-file-into-string *human-file*)))
        (do-seq (human (funcall *from-json* json) :value (tally -human-))
          (insert -human- human))))))

;;;; Interface

(defun model-test ()
  (with-pj-connection ()
    (show (size (contains -human- (map ("gender" "female")))))

    (show (@ (random-human) "name"))

    (let ((human (show (first (contains -human- (map ("name" "Marcella Marquez")))))))
      (let ((key (@ human "key"))
            (friends (@ human "friends")))
        (push-last friends (map ("name" "Horace Morris") ("id" (size friends))))
        (show (supersede -human- key (with human "friends" friends)))
        (show (@ (fetch -human- key) "friends"))
        (show (history -human- key))))

    (show (contains -human- (map ("tags" '("ut" "labore")))
                    :properties '("age" "tags")))
    (show (size (contains -human- (map ("isActive" t) ("age" 21)))))

    (show (size (having-property -human- "eyeColor")))
    (show (enumerate-property -human- "favoriteFruit")))
  (values))
