(in-package :postmodern)

;;; From postmodern/prepare.lisp with tweaks only to support named params list

(defun generate-prepared-wth-args (function-form query query-params format)
  (destructuring-bind (reader result-form) (reader-for-format format)
    (let ((base `(exec-prepared *database* (symbol-name statement-id) params ,reader)))
      `(let ((statement-id (next-statement-id))
             (query ,(real-query query)))
        (,@function-form (,@query-params)
          (let ((params (list ,@query-params)))
            (ensure-prepared *database* statement-id query)
            (,result-form ,base)))))))

(defmacro defprepared-with-args (name query query-params &optional (format :rows))
  (generate-prepared-wth-args `(defun ,name) query query-params format))

(export 'defprepared-with-args)
