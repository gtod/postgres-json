(in-package :postgres-json)

(defun symbol->json (symbol)
  (string-downcase (symbol-name symbol)))

(defun json->symbol (string)
  (ensure-symbol (string-upcase string)))

;;;; JSON related small functions

(defun obj (&rest args)
  "Return an 'equal hash-table consisting of pairs of ARGS."
  (let ((hash (make-hash-table :test #'equal)))
    (loop for (key val) on args by #'cddr do
      (setf (gethash key hash) val))
    hash))

;; from-json is simple yason:encode...
(defun to-json (object)
  "Convert a lisp OBJECT to a string of JSON, using YASON:ENCODE."
  (with-output-to-string (s)
    (yason:encode object s)))

(defun pp-json (object &key (stream *terminal-io*) (indent 4))
  "Pretty print lisp OBJECT as JSON to stream with specified indent."
  (let ((s (yason:make-json-output-stream stream :indent indent)))
    (yason:encode object s)))

(defun stash-key (key object)
  "If OBJECT is a hash-table add the pair \"key\" => KEY to a copy of
the hash-table and return it.  Otherwise just return OBJECT.  You
might like to write you own verson which can handle objects besides
hash tables."
  (if (hash-table-p object)
      (let ((copy (copy-hash-table object)))
        (setf (gethash "key" copy) key)
        copy)
      object))

(defun stash-key-destructive (key object)
  "If OBJECT is a hash-table add the pair \"key\" => KEY to the
hash-table and return it.  Otherwise just return OBJECT.  You might
like to write you own verson which can handle objects besides hash
tables."
  (when (hash-table-p object)
    (setf (gethash "key" object) key))
  object)

;;;; CLOS and closer-mop helpers

(defclass read-only () ()
  (:documentation "Inherit from class READ-ONLY to signal your intent
not to mutate any slots after object initialization."))

(defun slot-definitions (object)
  "Return the closer-mop slot-definitions for class of OBJECT."
  (closer-mop:compute-slots (class-of object)))

(defun slot-name (slot-definition)
  "Return SLOT-DEFINITION-NAME of SLOT-DEFINITION."
  (closer-mop:slot-definition-name slot-definition))

(defun slot-type (slot-definition)
  "Return SLOT-DEFINITION-TYPE of SLOT-DEFINITION."
  (closer-mop:slot-definition-type slot-definition))

;;;; Utility macros

(defmacro with-readers ((&rest readers) object &body body)
  "Bind the list of symbols in READERS to the current value of the
repective reader method, of the same name, invoked on OBJECT.  Then
evaluate BODY."
  (if (emptyp readers)
      `(progn ,@body)
      (once-only (object)
        `(let (,@(loop for reader in readers
                       collect `(,reader (,reader ,object))))
           ,@body))))

;;;; True utility functions, waiting for a real home

(defun sym (package-name &rest args)
  "Return symbol being the concatenation of upcasing ARGS.  See
ALEXANDRIA:FORMAT-SYMBOL for effect of PACKAGE-NAME."
  (format-symbol package-name "~:@(~{~A~}~)" args))

(defun sym-prefix (prefix symbol)
  (sym t prefix "-" symbol))

(defun sym-suffix (symbol suffix)
  (sym t symbol "-" suffix))
