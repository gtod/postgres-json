(defpackage :filter
  (:use :cl :postgres-json :postgres-json-model :postmodern)
  (:shadowing-import-from :postgres-json-model :get :delete :count)
  (:import-from :postgres-json :to-json))

(in-package :filter)

(defun create ()
  (unless (and pomo:*database* (pomo:connected-p pomo:*database*))
    (pomo:connect-toplevel "cusoon" "gtod" "" "localhost" :port 5433))
  (pomo:set-search-path "pgj_model,public")
  (unless (backend-exists-p)
    (create-backend))
  (unless (model-exists-p 'cat)
    (create-model 'cat)))

(defun insert-some-cats ()
  (insert 'cat (obj "name" "Joey" "coat" "tabby" "age" 7))
  (insert 'cat (obj "name" "Maud" "coat" "tortoiseshell" "age" 3))
  (insert 'cat (obj "name" "Max" "coat" "ginger" "age" 2)))

(defun cleanup ()
  (drop-model! 'cat))

;;; As far as I can tell we need to cast any JSON numeric field to an
;;; appropriate Postgres type (eg. int) before doing a numeric
;;; comparison on it...

;; This doesn't actually give you JSON strings
(defprepared cat-fields$
    (:order-by (:select
                (:as (:j "name") 'name)
                (:as (:j "age") 'age)
                :from 'cat
                :where (:> (:j "age" int) '$1))
               (:j "age" int)))

;; But this does
(defprepared cat-jdoc$
    (:order-by (:select (:jdoc)
                :from 'cat
                :where (:> (:j "age" int) '$1))
               (:j "age" int))
    :column)

;; And this
(defprepared cat-obj$
    (:order-by (:select (:jbuild "name" "age")
                :from 'cat
                :where (:> (:j "age" int) '$1))
               (:j "age" int))
    :column)

;; Containment
(defprepared cat-name-where$
    (:select (:j "name")
     :from 'cat
     :where (:@> (:jdoc) '$1))
    :column)

(defmacro show (query-form)
  `(pp-json (mapcar *from-json* ,query-form)))

(defun run ()
  (print (cat-fields$ 5))
  (terpri)
  (show (cat-jdoc$ 1))
  (terpri)
  (show (cat-obj$ 2))
  (terpri)
  (print (cat-name-where$ (to-json (obj "coat" "tabby")))))

#|

It's sort of broken because (:jdoc) makes no mention of the table
if we happen to want to select from two tables at once...

And setting search path explicitly rather than schema qualification??

Still, the first is a non issue for simple filtering and the second
is handle by model/query.lisp.

Updating: Could write a merge function for the server side but just
pull object back and do it in lisp for now.  Always in danger of
premature optimization...

|#
