(defpackage :filtering
  (:use :cl :postgres-json :postgres-json-model)
  (:shadowing-import-from :postgres-json-model :get :delete :count))

(in-package :filtering)

(defparameter *models* '(cat dog prime))

(defun create ()
  (unless (and pomo:*database* (pomo:connected-p pomo:*database*))
    (pomo:connect-toplevel "cusoon" "gtod" "" "localhost" :port 5433))
  (unless (backend-exists-p)
    (create-backend))
  (dolist (model *models*)
    (unless (model-exists-p model)
      (create-model model))))

(defun cleanup ()
  (dolist (model *models*)
    (drop-model! model)))

(defun insert-some ()
  (insert 'cat (obj "name" "Joey" "coat" "tabby" "age" 7
                    "likes" '("sunshine" "rain")
                    "trips" (obj "Barcelona" '(2014 2012 2009)
                                 "Kansas City" '(2013))))
  (insert 'cat (obj "name" "Maud" "coat" "tortoiseshell" "age" 3))
  (insert 'cat (obj "name" "Manny" "coat" "tortoiseshell" "age" 9))
  (insert 'cat (obj "name" "Max" "coat" "graphite" "age" 2))

  (insert 'dog (obj "name" "Rex" "coat" "graphite" "age" 3.5))

  (insert 'prime '(7 11 13)))

(defmacro show (form)
  `(progn
     (print ',form)
     (pp-json ,form)))


;; Any point trying to make these work on subsets of the relation?
;; What about on the old relation?

;; Containment operator
;; See 8.14.3 in Postgres manual 9.4
(defun filtering ()
  (show (filter 'cat :contain (obj "coat" "tortoiseshell")))

  ;; When using containment it's OK to omit spurious keys, but you must get
  ;; the nesting right.
  (show (filter 'cat :contain (obj "Kansas City" '(2013)))) ; No
  (show (filter 'cat :contain (obj "trips" (obj "Kansas City" '(2013))))) ; Works
  (show (filter 'cat :contain (obj "trips" (obj "Barcelona" '(2012 2009))))) ; Works!
  )

;; Top level key existence operator
;; Requires the (slower/bigger) jsonb_ops index
;; See 8.14.3/4 in Postgres manual 9.4
(defun existence ()
  (show (length (exists 'cat "name")))
  (show (exists 'cat "trips")))
