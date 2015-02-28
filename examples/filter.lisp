(defpackage :filtering
  (:use :cl :postgres-json))

(in-package :filtering)

(define-global-model cat -cat- (pgj-object-model))

(defun setup ()
  (ensure-top-level-connection)
  (ensure-backend -cat-))

(defun cleanup ()
  (drop-backend -cat-))

(defmacro show (form)
  `(progn
     (print ',form)
     (pp-json ,form)))

(defun insert-some-cats ()
  (insert -cat- (obj "name" "Joey" "coat" "tabby" "age" 7
                    "likes" '("sunshine" "rain")
                    "trips" (obj "Barcelona" '(2014 2012 2009)
                                 "Kansas City" '(2013))))
  (insert -cat- (obj "name" "Maud" "coat" "tortoiseshell" "age" 3))
  (insert -cat- (obj "name" "Manny" "coat" "tortoiseshell" "age" 9))
  (insert -cat- (obj "name" "Max" "coat" "graphite" "age" 2)))

;; Postgres top level property 'existence' operator, better called HAVING-PROPERTY
;; Requires the (slower/bigger) jsonb_ops index
;; See 8.14.3/4 in Postgres manual 9.4
(defun existence ()
  (show (length (having-property -cat- "name")))
  (show (having-property -cat- "trips")))

;; FILTER uses the Postgres Containment operator
;; See 8.14.3 in Postgres manual 9.4
;; A simple version could be implemented like so:
;; (define-json-query filter ((*to-json* contains))
;;   (:select 'jdoc
;;    :from 'cat
;;    :where (:@> 'jdoc contains)))
(defun filtering ()
  (show (contains -cat- (obj "coat" "tortoiseshell")))

  ;; When using containment it's OK to omit spurious keys, but you must get
  ;; the nesting right.
  (show (contains -cat- (obj "Kansas City" '(2013)))) ; No
  (show (contains -cat- (obj "trips" (obj "Kansas City" '(2013))))) ; Works
  (show (contains -cat- (obj "trips" (obj "Barcelona" '(2012 2009))))) ; Works!
  )
