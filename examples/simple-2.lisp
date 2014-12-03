;;;; Qualified access to the Postgres JSON model interface.

;;;; We still use the top level interface functions from
;;;; :postgres-json as none of these clash with common lisp exported
;;;; symbols...

(defpackage :simple-2
  (:use :cl :postgres-json))

(in-package :simple-2)

(defun create ()
  (unless (and pomo:*database* (pomo:connected-p pomo:*database*))
    ;; Change this to your Postmodern connect list...
    (pomo:connect-toplevel "cusoon" "gtod" "" "localhost" :port 5433))
  (unless (backend-exists-p)
    (create-backend))
  (unless (model-exists-p 'cat)
    (create-model 'cat)))

;; Because we are inserting
(defun insert-some-cats ()
  (pj:insert 'cat (obj "name" "Joey" "coat" "tabby"))
  (pj:insert 'cat (obj "name" "Maud" "coat" "tortoiseshell"))
  (pj:insert 'cat (obj "name" "Max" "coat" "ginger"))

  (format t "Cat keys: ~A~%" (pj:keys 'cat))

  (pj:delete 'cat (first (pj:keys 'cat)))

  (format t "Cat keys: ~A~%" (pj:keys 'cat))

  (pp-json (pj:get 'cat (second (pj:keys 'cat))))
  (terpri)

  (format t "Total cats: ~A~%" (pj:count 'cat))

  (let* ((key (first (pj:keys 'cat)))
         (cat (pj:get 'cat key)))
    (setf (gethash "age" cat) 7)
    (setf (gethash "likes" cat) '("rain" "sunflowers"))
    (pj:update 'cat key cat))

  (pp-json (pj:all 'cat))
  (terpri))

(defun cleanup ()
  (drop-model! 'cat))
