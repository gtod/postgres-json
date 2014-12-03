;;;; This is a bit hacky because I don't really grok the subtleties of
;;;; s-sql forms.  And there may be some gross escaping issues.
;;;; And the use of *jdoc* is a bit terrifying.

(in-package :s-sql)

(def-sql-op :jdoc ()
  (list (format nil "~A" postgres-json::*jdoc*)))

(def-sql-op :j (key &optional type)
  (let ((value `(,(format nil "~A->>" postgres-json::*jdoc*) ,@(sql-expand key))))
    (if type
        `("(" ,@value ")::" ,(to-type-name type))
        value)))

(def-sql-op :jbuild (&rest keys)
  (alexandria:flatten
   (list "json_build_object("
         (butlast (alexandria:flatten
                   (loop for key in keys
                         collect `(,@(sql-expand key) "," ,@(sql-expand `(:j ,key)) ", "))))
         ")")))
