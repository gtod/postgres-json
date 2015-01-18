;;;; Some little helper code for the human examples

(in-package :pj-human)

(defmacro with-pj-connection (() &body body)
  `(progn
     (assert *postmodern-connection*)
     (pomo:with-connection *postmodern-connection*
       (pomo:set-search-path *default-search-path*)
       ,@body)))

(defmacro show (form)
  `(progn
     (print ',form)
     (pp-json ,form)))

(defmacro with-keys ((&rest pairs) object &body body)
  (once-only (object)
    `(symbol-macrolet (,@(loop for pair in pairs
                               collect `(,(car pair) (gethash ,(cadr pair) ,object))))
       ,@body)))
