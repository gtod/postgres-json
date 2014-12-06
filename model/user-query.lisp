(in-package :postgres-json)

;;;; JSON queries syntactic sugar

;;; S-SQL largely supports the various JSON operators and "does the
;;; right thing" for function syntax such as (:json-build-object ...),
;;; see the Postgres 9.4 JSON doc on this and other functions.
;;; But it's a little verbose (I think) and so some more consise forms
;;; are defined here.  Everything else is still S-SQL but any list with
;;; car 'j-> or 'j->> or 'jbuild gets the expansions shown when
;;; used with DEFINE-QUERY.

;; Of course, we should use the full model name, but :as assignments
;; will also work!  maybe build only needs -> ?  Let's assume that for
;; now

#|
sugar                ; S-SQL

(j-> "id")           ; (:-> 'jdoc "id")
(j-> 'cat "id")      ; (:-> 'cat.jdoc "id").
(j->> 'c "id")       ; (:->> 'c.jdoc "id").

(jbuild ("id" "name"))      ; (:JSON-BUILD-OBJECT "id" (:-> 'JDOC "id") "name" (:-> 'JDOC "name"))
(jbuild ('cat "id" "name")) ; (:JSON-BUILD-OBJECT "id" (:-> 'CAT.JDOC "id") "name" (:-> 'CAT.JDOC "name"))

(jbuild ('cat "id" "name") ('dog "age"))                 ; OK, no duplicated keys
(jbuild ('cat "id" "name") ('dog ("dog-id" "id") "age")) ; Explicitly label duplicate key


j-> and j->> only take one or two args.  Examples above.

jbuild takes 1 or more lists as args:

If the list starts with a symbol (or quoted symbol) then that is used
to qualify all the jdoc accesses for the following keys.  Keys may be
strings (double duty as the label and the accessor) or a pair of
strings in a list, the first being the label and the second the accessor.

|#

(defun model-from-list-head (head)
  "You can write \(jbuild \('cat \"id\"\)\) or without the quote:
\(jbuild \(cat \"id\"\)\)."
  (if (or (symbolp head) (consp head))
      (if (symbolp head) head (cadr head))
      nil))

(defun nsubst-json-build (sugar tree)
  (let ((pairs '()))
    (flet ((nsubst-keys (column keys)
             (dolist (key keys)
               (let ((label (if (consp key) (car key) key))
                     (key (if (consp key) (cadr key) key)))
                 (push label pairs)
                 (push `(:-> ,column ,key) pairs)))))
      (dolist (form (cdr sugar))
        (let ((model (model-from-list-head (car form))))
          (if model
              (nsubst-keys `(quote ,(sym t model ".jdoc")) (cdr form))
              (nsubst-keys `(quote ,(sym t 'jdoc)) form))))
      (let ((new `(:json-build-object ,@(reverse pairs))))
        (nsubst new sugar tree :test #'equal))
      (values))))

(defun nsubst-json-op (sugar op tree)
  "Turn \(j-> \"id\"\) into \(:-> 'jdoc \"id\"\) and
\(j->> 'cat \"id\"\) into \(:->> 'cat.jdoc \"id\"\).
SUGAR is the incoming form to be transformed.  OP must be either :->
or :->> and TREE a tree in which we perform the NSUBST.  Nothing is
returned, TREE is destructively modifed."
  (flet ((nsubst-key (column key)
           (let ((new `(,op ,column ,key)))
             (nsubst new sugar tree :test #'equal))))
    (let ((model (model-from-list-head (cadr sugar))))
      (if model
          (nsubst-key `(quote ,(sym t model ".jdoc")) (caddr sugar))
          (nsubst-key `(quote ,(sym t 'jdoc)) (cadr sugar))))
    (values)))

(defun subst-sugar-in-query (form)
  "Replace all POSTGRES-JSON syntactic sugar variants in the S-SQL
FORM with their true S-SQL representations."
  (let ((tree (copy-tree form)))
    (flet ((handle (form)
             (when (and (consp form) (symbolp (car form)) (not (keywordp (car form))))
               (let ((head (symbol-name (car form))))
                 (cond ((string-equal "j->" head) (nsubst-json-op form :-> tree))
                       ((string-equal "j->>" head) (nsubst-json-op form :->> tree))
                       ((string-equal "jbuild" head) (nsubst-json-build form tree)))))))
      (walk-tree #'handle tree))
    tree))

;;;; JSON queries named parameter interpolation

;;; Named parameters to the query may be explicity interpolated.  Here
;;; we provide the names of explicit parameters to the query
;;; (filter email-regex) and these are replaced in the query form
;;; with '$1, '$2 etc...  You MUST provide explicit names for each
;;; parameter as these become the parameters of the ready-bookings$
;;; function.  You MAY write the params inline as shown below, or still
;;; write '$1, '$2 explicitly as in S-SQL.

;; (define-query ready-bookings$ (filter email-regex)
;;   (:order-by
;;    (:select (jbuild ("id" "name" "email"))
;;     :from 'booking
;;     :where (:and (:or (:@> 'jdoc filter))
;;                  (:~ (j->> "email") email-regex)))
;;    (:type (j->> "price") real)))

(defun subst-params-in-query (query-params query-form)
  "Walk the QUERY-FORM and substitute and symbol matching a symbol in
the list of symbols QUERY-PARAMS with $1, '$2, etc, based on their
order in QUERY-PARAMS."
  (let ((tree (copy-tree query-form)))
    (loop for param in query-params
          for i from 1
          do (nsubst `(quote ,(sym t "$" i)) param tree))
    tree))

;;;; Define query and support

;;; The 'containment' operator @> checks if some json you send as a
;;; query argument is contained in the jdoc column of a specifc model.
;;; Typically you don't have JSON in the lisp program, you have say a
;;; hash-table which needs to be serialized to JSON for this to work.
;;; So lifting the syntax idea from cl-ppcre:register-groups-bind you
;;; can specify in the parameter a function to map the actual argument
;;; from itself to something else.  So the example above assumes your
;;; 'filter arg is already JSON.  But we can write the params list as
;;; ((*to-json* filter) email-regex) to map filter from a hash-table
;;; to a JSON string when ready-bookings$ is called...

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
