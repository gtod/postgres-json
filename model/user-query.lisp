(in-package :postgres-json)

;;;; JSON queries syntactic sugar
;;;; See the User Guide for details

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
  "Define a Postmodern S-SQL based QUERY with name NAME, a symbol,
using both the 'JSON query named parameter interpolation' for each
symbol in the list QUERY-PARAMS and the 'JSON query syntactic sugar',
both documented in the User Guide.  In fact, elements of QUERY-PARAMS
may be lists of the form (function-designator &rest params) in which
case the PARAMS are still treated as parameters (in order) but at run
time FUNCTION-DESIGNATOR is called on each of the actual arguments of
the PARAMS to transform said arguments before use by the underlying
query.  For example: (foo (*to-json* bar baz) blot) is an acceptable
QUERY-PARAMS list, as long as *to-json* is funcallable."
  (with-unique-names (query-function)
    (multiple-value-bind (params transforms) (decompose-query-params-list query-params)
      (let ((s-sql-query (json-query-to-s-sql (car query) params)))
        `(let ((,query-function (prepare ,s-sql-query :column)))
           (defun ,name (,@params &key (from-json *from-json*))
             (let (,@transforms)
               (mapcar from-json (funcall ,query-function ,@params)))))))))
