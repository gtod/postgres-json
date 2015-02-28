(in-package :postgres-json)

;;;; Compatability functions for Fset

(defun mapper (fn sequence)
  "To support fset:seq we provide this wrapper around cl:mapcar."
  (mapcar fn sequence))

;;; Used in some test code

(defun geth (key place)
  (gethash key place))

(defun len (seq)
  (length seq))

;;;; JSON related small functions

(defun obj (&rest args)
  "Return an 'equal key/value hash-table consisting of pairs of ARGS.
For JSON use your keys must be Common Lisp strings."
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (key val) on args by #'cddr do
      (setf (gethash key hash) val))
    hash))

(defun from-json (string)
  "Parse the JSON string STRING and return the resulting lisp object."
  (yason:parse string :json-arrays-as-vectors t))

(defun to-json (object)
  "Convert a lisp OBJECT to a string of JSON."
  (with-output-to-string (s)
    (yason:encode object s)))

(defun pp-json (object &key (stream *standard-output*) (indent 4))
  "Pretty print lisp OBJECT as JSON to STREAM with specified INDENT."
  (fresh-line stream)
  (let ((s (yason:make-json-output-stream stream :indent indent)))
    (yason:encode object s)))

;;;; True utility functions and macros, waiting for a real home

(defmacro first-value (form)
  `(nth-value 0 ,form))

(defun sym (package-name &rest args)
  "Return symbol being the concatenation of upcasing ARGS.  See
ALEXANDRIA:FORMAT-SYMBOL for effect of PACKAGE-NAME."
  (format-symbol package-name "~:@(~{~A~}~)" args))

(defun sym-prefix (prefix symbol)
  (sym t prefix "-" symbol))

(defun sym-suffix (symbol suffix)
  (sym t symbol "-" suffix))

(defun walk-tree (fun tree)
  "Walk TREE and call FUN at each node.  Thanks to Lisp Tips."
  (subst-if t (constantly nil) tree :key fun))
