(defpackage :simple
  (:use :cl :postgres-json))

(in-package :simple)

(defun create ()
  (ensure-top-level-connection)
  (ensure-backend)
  (ensure-model 'cat))

(defun insert-some-cats ()
  (insert 'cat (obj "name" "Joey" "coat" "tabby"))
  (insert 'cat (obj "name" "Maud" "coat" "tortoiseshell"))
  (insert 'cat (obj "name" "Max" "coat" "ginger"))

  (format t "Cat keys: ~A~%" (keys 'cat))

  (excise 'cat (first (keys 'cat)))

  (format t "Cat keys: ~A~%" (keys 'cat))

  (pp-json (fetch 'cat (second (keys 'cat))))

  (format t "~%Total cats: ~A~%" (tally 'cat))

  (let* ((key (first (keys 'cat)))
         (cat (fetch 'cat key)))
    (setf (gethash "age" cat) 7)
    (setf (gethash "likes" cat) '("rain" "sunflowers"))
    (update 'cat key cat))

  (pp-json (fetch-all 'cat)))

(defun drop ()
  (drop-model 'cat))
