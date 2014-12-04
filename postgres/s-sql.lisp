;;;; This is a bit hacky because I don't really grok the subtleties of
;;;; s-sql forms.  And there may be some gross escaping issues.

(in-package :s-sql)

(def-sql-op :j (jdoc key &optional type)
  (let ((value `(,(format nil "~A->>" jdoc) ,@(sql-expand key))))
    (if type
        `("(" ,@value ")::" ,(to-type-name type))
        value)))

(def-sql-op :jbuild (jdoc &rest keys)
  (alexandria:flatten
   (list "json_build_object("
         (butlast (alexandria:flatten
                   (loop for key in keys
                         collect `(,@(sql-expand key) "," ,@(sql-expand `(:j ,jdoc ,key)) ", "))))
         ")")))
