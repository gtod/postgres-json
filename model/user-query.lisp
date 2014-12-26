(in-package :postgres-json)

;;;; JSON queries syntactic sugar

(defun model-from-list-head (head)
  "You can write \(jbuild \('cat \"id\"\)\) or without the quote:
\(jbuild \(cat \"id\"\)\).  If HEAD is a symbol or a quoted
symbol, return said symbol."
  (if (or (symbolp head) (consp head))
      (if (symbolp head) head (cadr head))
      nil))

;;; We (and you can) macroexpand these macros to see what they
;;; transform into, but they must not be evaluated, the code is not
;;; lisp.

;; Maybe a bridge too far from S-SQL
;; (defmacro qn (model &optional as)
;;   "Syntactic sugar to qualify the name of MODEL, a symbol in
;; *PGJ-SCHEMA*.  Wrap in an S-SQL :as form using MODEL as the
;; abbreviation for the qualified MODEL, or use the symbol AS as the
;; abbreviation if it is supplied."
;;   `(:as ',(sym t *pgj-schema* "." model) ',(if as as model)))

(defmacro j-> (form1 &optional form2)
    "S-SQL syntactic sugar to turn a single string FORM1 into a
Postgres -> operation using the default JSON column 'jdoc and the
property FORM1; or to turn a symbol FORM1 and string FORM2 into a ->
operation using the specified JSON column FORM1 and the property
FORM2."
  (if form2
      (let ((model (model-from-list-head form1)))
        `(:-> ',(sym t model ".jdoc") ,form2))
      `(:-> 'jdoc ,form1)))

(defmacro j->> (form1 &optional form2)
  "S-SQL syntactic sugar to turn a single string FORM1 into a Postgres
->> operation using the default JSON column 'jdoc and the property
FORM1; or to turn a symbol FORM1 and string FORM2 into a ->> operation
using the specified JSON column FORM1 and the property FORM2."
  (if form2
      (let ((model (model-from-list-head form1)))
        `(:->> ',(sym t model ".jdoc") ,form2))
      `(:->> 'jdoc ,form1)))

(defmacro to-jsonb (form)
  "S-SQL syntactic sugar to cast FORM to the Postgres jsonb type."
  `(:type (:to-json ,form) ,(sym t 'jsonb)))

(defmacro jbuild (&rest key-forms)
  "S-SQL syntactic sugar to create a new Postgres JSON object from the
KEY-FORMS.  Each KEY-FORM is a list.  In the simplest and first case
it may be a list of strings, said strings indicating properties of the
top level JSON object in the 'jdoc column of the query; the properties
and their values will be returned by JBUILD, in a fresh JSON object.
In the second case the list may start with a symbol \(or a quoted
symbol\) in which case the following strings indicate properties of
the top level JSON document in the 'jdoc column in the DB table named
by the symbol.  Now, a la `with-slots`, each string in the list may
itself be replaced by a list of two strings, the first being the
resulting property name in the object returned by JBUILD, the second
being the accessor property for the top level JSON object in the 'jdoc
column.  This flexibility is required because we are building a JSON
object and cannot have duplicate properties so if we need the \"id\"
property from both a `cat` and a `dog` model, one of them needs to be
relabeled."
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
;;;; See the User Guide for details

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
  "Transform a JSON QUERY-FORM into S-SQL, interpolating the list of
PARAMS, if any.  The acceptaple format of QUERY-FORM is documented
in the User Guide under 'User defined JSON queries'."
  (subst-json-sugar (subst-params-in-query params query-form)))

;;;; Define json query and support
;;;; See the User Guide for details

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
  "Define a Postmodern S-SQL based QUERY with name NAME, a symbol.
QUERY may use the macro forms j->, j->> jbuild and to-json, documented
separately.  Elements of QUERY-PARAMS may be symbols, the number and
order of said symbols serving to define the parameters the query will
be supplied with at run time.  Additionally, any occurence of a symbol
from the QUERY-PARAMS list in the QUERY from proper will be replaced
with '$1, '$2 etc. as appropriate based on the order of QUERY-PARAMS.
In this way your queries may use named parameters, but this is not
mandatory.

Furthermore, a la `cl-ppcre:register-groups-bind`, any element of the
QUERY-PARAMS list may itself be a list of the form
\(function-designator &rest params\) in which case the PARAMS are
still treated as parameters, in order, but at run time
FUNCTION-DESIGNATOR is called on each of the actual arguments of the
PARAMS to transform said arguments before use by the underlying query.
For example `\(foo \(*to-json* bar baz\) blot\)` is an acceptable
QUERY-PARAMS list, as long as *to-json* is funcallable.  bar and baz
will be replaced by the result of funcalling *to-json* on them,
repectively.

The Postmodern result format is always `:column` and so you must
ensure that each row produces just a single datum, being a valid
Postgres JSON type.  In practice this means either i) returning the
column named `jdoc` in any model, which is the entire JSON document,
or ii) using the `jbuild` macro to build some JSON on the fly."
  (with-unique-names (query-function)
    (multiple-value-bind (params transforms) (decompose-query-params-list query-params)
      (let ((s-sql-query (json-query-to-s-sql (car query) params)))
        `(let ((,query-function (prepare ,s-sql-query :column)))
           (defun ,name (,@params &key (from-json *from-json*))
             (let (,@transforms)
               (mapcar from-json (funcall ,query-function ,@params)))))))))
