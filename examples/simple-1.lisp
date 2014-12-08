;;;; Unqualified access to the Postgres JSON model interface
;;;; See simple-2 for qualified access

(defpackage :simple-1
  (:use :cl :postgres-json :postgres-json-model)
  (:shadowing-import-from :postgres-json-model :get :delete :count))

(in-package :simple-1)

(defun create ()
  (unless (and pomo:*database* (pomo:connected-p pomo:*database*))
    ;; Change this to your Postmodern connect list...
    (pomo:connect-toplevel "cusoon" "gtod" "" "localhost" :port 5433))
  (unless (backend-exists-p)
    (create-backend))
  (unless (model-exists-p 'cat)
    (create-model 'cat)))

(defun insert-some-cats ()
  (insert 'cat (obj "name" "Joey" "coat" "tabby"))
  (insert 'cat (obj "name" "Maud" "coat" "tortoiseshell"))
  (insert 'cat (obj "name" "Max" "coat" "ginger"))

  (format t "Cat keys: ~A~%" (keys 'cat))

  (delete 'cat (first (keys 'cat)))

  (format t "Cat keys: ~A~%" (keys 'cat))

  (pp-json (get 'cat (second (keys 'cat))))

  (format t "~%Total cats: ~A~%" (count 'cat))

  (let* ((key (first (keys 'cat)))
         (cat (get 'cat key)))
    (setf (gethash "age" cat) 7)
    (update 'cat key cat))

  (pp-json (get-all 'cat)))

(defun drop ()
  (drop-model! 'cat))
