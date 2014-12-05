(in-package :postgres-json)

;;; S-SQL largely supports the various JSON operators and "does the
;;; right thing" for function syntax such as (:json-build-object ...)
;;; below (see the Postgres JSON doc on this and other functions).
;;; But it's a little verbose (I think) and so some more consise
;;; forms may be used as shown here.  It's all still S-SQL but any
;;; list with car 'j-> or 'j->> gets the expansions shown when used
;;; to define the query with DEFINE-QUERY.

;; (subst-sugar-in-query
;;                 '((j-> "id")
;;                   (j->> "id")
;;                   (j-> 'c.jdoc "id")
;;                   (j->> 'c.jdoc "id")
;;                   (j-> "id" "name")
;;                   (j->> "id" "name")
;;                   (j-> 'd.jdoc "id" "name")
;;                   (j->> 'd.jdoc "id" "name" "age")))
;; ((:-> 'JDOC "id")
;;  (:->> 'JDOC "id")
;;  (:-> 'C.JDOC "id")
;;  (:->> 'C.JDOC "id")
;;  (:JSON-BUILD-OBJECT "id" (:-> 'JDOC "id") "name" (:-> 'JDOC "name"))
;;  (:JSON-BUILD-OBJECT "id" (:->> 'JDOC "id") "name" (:->> 'JDOC "name"))
;;  (:JSON-BUILD-OBJECT "id" (:-> 'D.JDOC "id") "name" (:-> 'D.JDOC "name"))
;;  (:JSON-BUILD-OBJECT "id" (:->> 'D.JDOC "id") "name" (:->> 'D.JDOC "name") "age" (:->> 'D.JDOC "age")))

;;; Furthermore, named parameters to the query may be explicity
;;; interpolated.  Here we provide the names of explicit parameters
;;; to the query (filter email-regex) and these are replaed in the
;;; query form with '$1, '$2 etc...

;; (define-query ready-bookings$ (filter email-regex)
;;   (:order-by
;;    (:select (j->> "id" "name" "email")
;;     :from 'booking
;;     :where (:and (:or (:@> 'jdoc filter))
;;                  (:~ (j->> "email") email-regex)))
;;    (:type (j->> "price") real)))

(defun nsubst-json-builder (form op tree)
  (labels ((nsubst-key (column key)
             (let ((new `(,op ,column ,key)))
               (nsubst new form tree :test #'equal)))
           (nsubst-keys (column keys)
             (let ((pairs '()))
               (dolist (key keys)
                 (push key pairs)
                 (push `(,op ,column ,key) pairs))
               (let ((new `(:json-build-object ,@(reverse pairs))))
                 (nsubst new form tree :test #'equal))))
           (explicit-column-p ()
             (listp (cadr form)))
           (single-key-p ()
             (= (if (explicit-column-p) 3 2) (length form))))
    (if (explicit-column-p)
        (if (single-key-p)
            (nsubst-key (cadr form) (caddr form))
            (nsubst-keys (cadr form) (cddr form)))
        (if (single-key-p)
            (nsubst-key '(quote jdoc) (cadr form))
            (nsubst-keys '(quote jdoc) (cdr form))))))

(defun subst-sugar-in-query (form)
  "Replace all POSTGRES-JSON syntactic sugar variants in the S-SQL
FORM with their true S-SQL representations."
  (let ((tree (copy-tree form)))
    (flet ((handle (form)
             (when (and (listp form) (symbolp (car form)))
               (let ((head (symbol-name (car form))))
                 (cond ((string-equal "j->" head) (nsubst-json-builder form :-> tree))
                       ((string-equal "j->>" head) (nsubst-json-builder form :->> tree)))))))
      (walk-tree #'handle tree))
    tree))

(defun subst-params-in-query (query-params query-form)
  "Walk the QUERY-FORM and substitute and symbol matching a symbol in
the list of symbols QUERY-PARAMS with $1, '$2, etc, based on their
order in QUERY-PARAMS."
  (let ((tree (copy-tree query-form)))
    (loop for param in query-params
          for i from 1
          do (nsubst `(quote ,(sym t "$" i)) param tree))
    tree))

(defmacro define-query (name (&rest query-params) &body query)
  "Define a Postmodern S-SQL based QUERY with name NAME, a symbol,
using both the named parameters syntax for each symbol in the list
QUERY-PARAMS and the JSON access syntactic sugar."
  (let* ((form0 (subst-params-in-query query-params (car query)))
         (form (subst-sugar-in-query form0)))
    `(defprepared-with-args ,name ,form ,query-params :column)))

(define-query ready-bookings$ (filter email-regex)
  (:order-by
   (:select (j->> "id" "name" "email")
    :from 'booking
    :where (:and (:or (:@> 'jdoc '$1))
                 (:~ (j->> "email") email-regex)))
   (:type (j->> "price") real)))
