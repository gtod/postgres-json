(in-package :postgres-json)

;;; S-SQL largely supports the various JSON operators and "does the
;;; right thing" for function syntax such as (:json-build-object ...)
;;; below (see the Postgres JSON doc on this and other functions).
;;; But it's a little verbose (I think) and so some more consise
;;; forms may be used as shown here.  It's all still S-SQL but any
;;; list with car 'j-> or 'j->> gets the expansions shown when used
;;; to define the query with DEFINE-QUERY.

;;; See the Postgres 9.4 manual for information on JSON operators and
;;; functions.

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
;;; to the query (filter email-regex) and these are replaced in the
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

(defun decompose-query-params-list (query-params)
  "Turns (foo (*to-json* bar baz) blot) into two values:
(FOO BAR BAZ BLOT) and
((BAR (FUNCALL *TO-JSON* BAR)) (BAZ (FUNCALL *TO-JSON* BAZ)))
for use in the define-query macro."
    (let ((params '())
          (transforms '()))
      (dolist (form query-params)
        (if (consp form)
            (let ((function (first form)))
              (dolist (param (rest form))
                (push param params)
                (push `(,param (funcall ,function ,param)) transforms)))
            (push form params)))
      (values (nreverse params) (nreverse transforms))))

(defmacro define-query (name (&rest query-params) &body query)
  "Define a Postmodern S-SQL based QUERY with name NAME, a symbol,
using both the 'named parameters syntax' for each symbol in the list
QUERY-PARAMS and the 'JSON access syntactic sugar', both documented in
model/user-query.lisp.  In fact, elements of QUERY-PARAMS may be lists
of the form (function-designator &rest params) in which case the
PARAMS are still treated as parameters (in order) but at run time
FUNCTION-DESIGNATOR is called on each of the actual arguments of the
PARAMS to transfrom said arguments before use by the underlying query.
For example: (foo (*to-json* bar baz) blot) is an acceptable
QUERY-PARAMS list, as long as *to-json* is funcallable."
  (multiple-value-bind (params transforms) (decompose-query-params-list query-params)
    (let ((s-sql-query (subst-sugar-in-query (subst-params-in-query params (car query)))))
      (with-unique-names (query-function)
        `(let ((,query-function (prepare ,s-sql-query :column)))
           (defun ,name (,@params &key (from-json *from-json*))
             (let (,@transforms)
               (mapcar from-json (funcall ,query-function ,@params)))))))))


;; (define-query ready-bookings$ (filter email-regex)
;;   (:order-by
;;    (:select (j->> "id" "name" "email" "state")
;;     :from 'booking
;;     :where (:and (:or (:@> 'jdoc filter))
;;                  (:~ (j->> "email") email-regex)))
;;    (:type (j->> "price") real)))

;; (define-query animals$ ()
;;   (:select (j->> 'c.jdoc "name" "coat")
;;    :from (:as 'cat 'c)
;;    :inner-join (:as 'dog 'd)
;;    :on (:= (j->> 'c.jdoc "name") (j->> 'd.jdoc "name"))))

;; (defun ready-bookings (model filter-object email-regex
;;                        &key (to-json *to-json*) (from-json *from-json*))
;;   (ensure-model-query model 'ready-bookings$)
;;   (ensure-transaction-level (filter read-committed-ro)
;;     (mapcar from-json (ready-bookings$ (funcall to-json filter-object) email-regex))))


;; Helpers should provide some syntactic sugar
;; and do the automatic to-json for incoming filters
;; and the from-json from returing JSON
;; Maybe we specify if it *returns* json (the default)
;; And maybe we specific incoming params that need to-json

;;; You know, key and jdoc as mode params are just a wast of time.
;;; They should be hard coded, what hope have we got of writing queries?
;;; But what of compound primary keys?
;;; Do we need model params at all?  table and old table can be made on demand
;;; from the model symbol!!
;;; Well, we do need key-type (eg. uuid) and (maybe) jdoc-type.  But would
;;; be simpler if it was just key (compound) and key-type...

;;; As far as I can tell we need to cast any JSON numeric field to an
;;; appropriate Postgres type (eg. int) before doing a numeric
;;; comparison on it...
