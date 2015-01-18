;;; Evaluate something like the following before proceeding:
;;; (setf *postmodern-connection* '("mydb" "myname" "" "localhost"))

;;; Now evaluate (setup), (insert-some-ungulates),
;;; (insert-invalid-sheep), (fetch-sheep), (pp-json *)

;;; Maybe quickload JSOWN, recompile and call (fetch-sheep) again...

;;; Maybe call (insert-some-ungulates) again to see the sheep primary
;;; key errors.

;;; (change-sheep-index) merely demonstrates syntax for selecting
;;; model GIN indexes at run time.

(defpackage :customize
  (:use :cl :postgres-json))

(in-package :customize)

;;; 1. Give model its own sequence, see SETUP below
;;; where we make the sequence
(define-global-model goat -goat- (pgj-object-model))

(defmethod model-sequence ((model goat))
  'goat-seq)

;;; 2. Give model a different key type
;;; Presently this means you need to explicitly supply the key with
;;; each insert --- see below.
(define-global-model sheep -sheep- (pgj-object-model))

(defmethod model-key-type ((model sheep))
  'text)

;;; 3. Rudimentary validation
(defmethod serialize :before ((model sheep) (object hash-table))
  (assert (nth-value 1 (gethash "name" object))))

;;; 4. Customize deserialization
;;; Do (ql:quickload :jsown) and recompile this file to test.
;;; In fact, you can also just bind or set *from-json* to achieve
;;; the same end, but not easily on a per model basis...
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:system-registered-p :jsown)
    (pushnew :jsown *features*)))

#+jsown
(defmethod deserialize ((model sheep) jdoc)
  (log:info "Deserializing sheep using JSOWN")
  (jsown:parse jdoc))

(defun setup ()
  (ensure-top-level-connection)
  (unless (pomo:sequence-exists-p 'goat-seq)
    (create-db-sequence 'goat-seq))
  (ensure-backend -goat-)
  (ensure-backend -sheep-))

(defun insert-some-ungulates ()
  (insert -goat- (obj "name" "Billy" "coat" "white"))
  (insert -goat- (obj "name" "Grumpy" "coat" "black"))

  (format t "Goat keys: ~A~%" (keys -goat-))

  (insert -sheep- (obj "name" "Shaun" "coat" "white") "shaun")
  (insert -sheep- (obj "name" "Harold" "coat" "white") "harold")

  (format t "Sheep keys: ~A~%" (keys -sheep-)))

(defun insert-invalid-sheep ()
  (insert -sheep- (obj "nickname" "Boris" "coat" "brown") "boris"))

;; If JSOWN is not loaded Yason will return a list of hash tables so
;; then just do (pp-json *).  For JSOWN, you are on your own.
(defun fetch-sheep ()
  (fetch-all -sheep-))

(defun change-sheep-index ()
  ;; Faster but does not support Postgres ? operator, used by
  ;; HAVING-PROPERTY for example
  (use-gin-index -sheep- :jsonb-path-ops)

  ;; The default
  (use-gin-index -sheep- :jsonb-ops))

;;;; Cleanup

(defun cleanup ()
  (excise-all -goat-)
  (excise-all -sheep-))

(defun drop ()
  (drop-backend -goat-)
  (drop-backend -sheep-))
