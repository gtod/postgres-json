;;; Evaluate something like the following before proceeding:
;;; (setf *postmodern-connection* '("mydb" "myname" "" "localhost"))

;;; Evaluate (setup), (insert-some-pigs) and (local-rat)

(defpackage :simple
  (:use :cl :postgres-json))

(in-package :simple)

(define-global-model pig -pig- (pgj-object-model))

(defun setup ()
  (ensure-top-level-connection)
  (ensure-backend -pig-))

(defun insert-some-pigs ()
  (insert -pig- (obj "name" "Leon" "coat" "pink"))
  (insert -pig- (obj "name" "Roger" "coat" "Tan"))
  (insert -pig- (obj "name" "Zebedee" "coat" "black pied"))

  (format t "Pig keys: ~A~%" (keys -pig-))

  (excise -pig- (first (keys -pig-)))

  (format t "Pig keys: ~A~%" (keys -pig-))

  (pp-json (fetch -pig- (second (keys -pig-))))

  (format t "~%Total pigs: ~A~%" (tally -pig-))

  (let* ((key (first (keys -pig-)))
         (pig (fetch -pig- key)))
    (setf (gethash "age" pig) 7)
    (setf (gethash "likes" pig) '("rain" "sunflowers"))
    (supersede  -pig- key pig))

  (pp-json (fetch-all -pig-)))

;; Models need not be global.  Macroexpand the define-global-model
;; form above to see what it really is.  But because there are no
;; slots in PGJ-OBJECT-MODEL (or relatives) *all instances are always
;; the same* which is why making the single global instance is
;; sensible.
(defun local-rat ()
  (let ((rat (make-instance (defclass rat (pgj-object-model) ()))))
    (ensure-backend rat)
    (insert rat (obj "name" "Ratty" "color" "black"))
    (format t "Rats: ~A~%" (tally rat))))

(defun drop ()
  (drop-backend -pig-))
