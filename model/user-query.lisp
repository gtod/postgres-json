(in-package :postgres-json)

;;;; JSON queries syntactic sugar

;;; S-SQL largely supports the various JSON operators and "does the
;;; right thing" for function syntax such as (:json-build-object ...),
;;; see the Postgres 9.4 JSON doc on this and other functions.  But
;;; it's a little verbose (I think) and so some more consise forms are
;;; defined here.  Everything else is still S-SQL but any list with
;;; car 'j-> or 'j->> or 'jbuild or 'to-json gets the expansions shown
;;; when used with DEFINE-QUERY.

;; You can use the full model name such as 'cat but :as assignments
;; will also work.  The quote on 'cat or 'c is optional when using any
;; of these j operators...

;; Maybe jbuild only needs -> ?  Let's assume that for now.

#|

You can simply macroexpand the LHS form to get the RHS...

sugar                ; S-SQL

(j-> "id")           ; (:-> 'jdoc "id")
(j-> 'cat "id")      ; (:-> 'cat.jdoc "id").
(j->> 'c "id")       ; (:->> 'c.jdoc "id").

(to-jsonb 1)         ; (:TYPE (:TO-JSON 1) JSONB)

(jbuild ("id" "name"))      ; (:JSON-BUILD-OBJECT "id" (:-> 'JDOC "id") "name" (:-> 'JDOC "name"))
(jbuild ('cat "id" "name")) ; (:JSON-BUILD-OBJECT "id" (:-> 'CAT.JDOC "id") "name" (:-> 'CAT.JDOC "name"))

;; OK, no duplicated keys
(jbuild ('cat "id" "name") ('dog "age")) ->
(:JSON-BUILD-OBJECT "id"  (:-> 'CAT.JDOC "id") "name" (:-> 'CAT.JDOC "name")
                    "age" (:-> 'DOG.JDOC "age"))

;; Explicitly label duplicate key
(jbuild ('cat "id" "name") ('dog ("dog-id" "id") "age"))
(:JSON-BUILD-OBJECT "id"     (:-> 'CAT.JDOC "id") "name" (:-> 'CAT.JDOC "name")
                    "dog-id" (:-> 'DOG.JDOC "id") "age"  (:-> 'DOG.JDOC "age"))

j-> and j->> only take one or two args. As shown above the relation
name is optional, unless of course this would lead to ambiguity.
The realtion name need not be quoted.  So 'cat or cat are both fine.

to-jsonb takes a single form as an argument, which will be converted
by the Postgres TO-JSON function and then cast to the Postgres jsonb
type.

jbuild takes 1 or more lists as args:

If the list starts with a symbol (or quoted symbol) then that is used
to qualify all the jdoc accesses for the following keys.  Keys may be
strings (double duty as the label and the accessor) or a pair of
strings in a list, the first being the label and the second the
accessor.

|#

(defun model-from-list-head (head)
  "You can write \(jbuild \('cat \"id\"\)\) or without the quote:
\(jbuild \(cat \"id\"\)\)."
  (if (or (symbolp head) (consp head))
      (if (symbolp head) head (cadr head))
      nil))

;;; We (and you can) macroexpand these macros to see what they
;;; transform into, but they must not be evaluated, the code is not
;;; lisp.

(defmacro j-> (form1 &optional form2)
  (if form2
      (let ((model (model-from-list-head form1)))
        `(:-> ',(sym t model ".jdoc") ,form2))
      `(:-> 'jdoc ,form1)))

(defmacro j->> (form1 &optional form2)
  (if form2
      (let ((model (model-from-list-head form1)))
        `(:->> ',(sym t model ".jdoc") ,form2))
      `(:->> 'jdoc ,form1)))

(defmacro to-jsonb (form)
  `(:type (:to-json ,form) ,(sym t 'jsonb)))

(defmacro jbuild (&rest key-forms)
  (let ((pairs '()))
    (flet ((nsubst-keys (column keys)
             (dolist (key keys)
               (let ((label (if (consp key) (car key) key))
                     (key (if (consp key) (cadr key) key)))
                 (push label pairs)
                 (push `(:-> ,column ,key) pairs)))))
      (dolist (form key-forms)
        (let ((model (model-from-list-head (car form))))
          (if model
              (nsubst-keys `(quote ,(sym t model ".jdoc")) (cdr form))
              (nsubst-keys `(quote jdoc) form))))
      `(:json-build-object ,@(reverse pairs)))))

(defparameter *json-sugar-list-heads* '("j->" "j->>" "jbuild" "to-jsonb"))

;; Based on Graham's On Lisp 5.6
(defun subst-json-sugar (tree)
  "Replace all POSTGRES-JSON syntactic sugar variants in TREE with
their S-SQL representations."
  (if (atom tree)
      tree
      (let ((car (car tree)))
        (when (and (symbolp car) (not (keywordp car)))
          (let ((head (symbol-name car)))
            (when (member head *json-sugar-list-heads* :test #'string-equal)
              (setf tree (macroexpand-1 tree)))))
        (cons (subst-json-sugar (car tree))
              (if (cdr tree) (subst-json-sugar (cdr tree)))))))

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

(defun json-query-to-s-sql (query-form &optional params)
  "Transform our JSON QUERY-FORM into S-SQL, interpolating the list of
PARAMS, if any."
  (subst-json-sugar (subst-params-in-query params query-form)))

;;;; Define json query and support

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

(defmacro define-json-query (name (&rest query-params) &body query)
  "Define a Postmodern S-SQL based QUERY with name NAME, a symbol,
using both the 'named parameters interpolation' for each symbol in the
list QUERY-PARAMS and the 'JSON queries syntactic sugar', both
documented in model/user-query.lisp.  In fact, elements of
QUERY-PARAMS may be lists of the form (function-designator &rest
params) in which case the PARAMS are still treated as parameters (in
order) but at run time FUNCTION-DESIGNATOR is called on each of the
actual arguments of the PARAMS to transform said arguments before use
by the underlying query.  For example: (foo (*to-json* bar baz) blot)
is an acceptable QUERY-PARAMS list, as long as *to-json* is
funcallable."
  (with-unique-names (query-function)
    (multiple-value-bind (params transforms) (decompose-query-params-list query-params)
      (let ((s-sql-query (json-query-to-s-sql (car query) params)))
        `(let ((,query-function (prepare ,s-sql-query :column)))
           (defun ,name (,@params &key (from-json *from-json*))
             (let (,@transforms)
               (mapcar from-json (funcall ,query-function ,@params)))))))))
