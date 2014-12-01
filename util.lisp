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

(defun stash-id (id hash)
  "Add the pair \"id\" => ID to a copy of the hash-table HASH and
return it.  (Clearly if you intend to use this when calling INSERT
your objects must all be hash tables)."
  (let ((copy (copy-hash-table hash)))
    (setf (gethash "id" copy) id)
    copy))

;;;; closer-mop helpers

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

(defun maphash-symbols-to-strings (hash &key (test #'equal))
  (let ((new (make-hash-table :test test :size (hash-table-size hash))))
    (maphash (lambda (key value)
               (setf (gethash key new) (symbol-name value)))
             hash)
    new))

(defun maphash-strings-to-symbols (hash &key (test #'equal))
  (let ((new (make-hash-table :test test :size (hash-table-size hash))))
    (maphash (lambda (key value)
               (setf (gethash key new) (ensure-symbol value)))
             hash)
    new))
(defun sym-prefix (prefix symbol)
  (sym t prefix "-" symbol))

(defun sym-suffix (symbol suffix)
  (sym t symbol "-" suffix))
