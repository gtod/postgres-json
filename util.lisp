(in-package :postgres-json)

(defun sym (package-name &rest args)
  "Return symbol being the concatenation of upcasing ARGS.  See
ALEXANDRIA:FORMAT-SYMBOL for effect of PACKAGE-NAME."
  (format-symbol package-name "~:@(~{~A~}~)" args))

(defun sequence-op-name (sequence-op name schema package-name)
  "Return a symbol interned in the package denoted by PACKAGE-NAME
with name <SEQUENCE-OP>-<SCHEMA>-<NAME>$ where <...> stands for string
interpolation."
  (sym package-name sequence-op "-" schema "-" name "$"))
