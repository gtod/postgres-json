(in-package :postgres-json)

;; Do we need to delete it first?
(defmacro def-model-package (name)
  `(progn
     (when-let (package (find-package ',name))
       (delete-package package))
     (defpackage ,name
       (:use ,@*model-use-symbols*)
       (:shadow ,@*model-shadow-symbols*)
       (:export ,@*model-export-symbols*))))

(defmacro defprepare-model-op (name (package-name) &body body)
  "A DEFPREPARED like form for defining a Postmodern statement
function with name NAME in the model package denoted by PACKAGE-NAME,
a symbol."
  `(defprepared ,(ensure-symbol name package-name) ,@body))

(defmacro defun-model-op (name (package-name) args &body body)
  "A DEFUN like form for defining a function with name NAME in the
model package denoted by PACKAGE-NAME, a symbol.  The function's
symbol may be exported by setting EXPORT-P true."
  `(with-fbound-symbols-in-package (,package-name)
     (defun ,(ensure-symbol name package-name) (,@args) ,@body)))

;;; This is maybe more funcky that I'd like, in the sense that I
;;; wonder if there is not a much simpler way to accomplish the same
;;; thing.  Suggestions on a postcard please...
(defmacro with-fbound-symbols-in-package ((package-name) &body body)
  "When defining a DEFUN-MODEL-OP form we want to write, say, (delete$ id).
when we really mean (cat::delete$ id), if our model is called 'cat,
for example.  This macro MACROLETs any present-symbols (in the LOOP
sense) in the scope of BODY that are also fbound in our model
package (denoted by PACKAGE-NAME) to make precisely this
transformation.  You can always macroexpand any DEFUN-MODEL-OPS forms
and then macroexpand the WITH-FBOUND-SYMBOLS-IN-PACKAGE form to see
the macrolets in force."
  `(macrolet (,@(loop for symbol being each present-symbol of package-name
                      when (fboundp symbol)
                        collect `(,(ensure-symbol symbol :postgres-json) (&rest args)
                                  `(,',(ensure-symbol symbol package-name) ,@args))))
     ,@body))
