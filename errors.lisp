(in-package :postgres-json)

;;;; Ancestor condition for entire library

(define-condition postgres-json-error (error) ())

;;;; Database conditions

(define-condition postgres-json-database-error (postgres-json-error) ())

(define-condition database-safety-net (postgres-json-database-error)
  ((attempted-to :initarg :attempted-to :reader attempted-to))
  (:report (lambda (condition stream)
             (format stream "To save you from yourself I refuse to: ~A"
                     (attempted-to condition)))))
