(in-package :postgres-json)

;;;; Ancestor condition for entire library

(define-condition postgres-json-error (error) ())

;;;; Database conditions

(define-condition postgres-json-database-error (postgres-json-error) ())

(define-condition database-safety-net (postgres-json-database-error)
  ((attempted-to :initarg :attempted-to :reader attempted-to)
   (suggestion :initarg :suggestion :reader suggestion))
  (:report (lambda (condition stream)
             (format stream "To save you from yourself I refuse to: ~A.~%May I suggest you: ~A."
                     (attempted-to condition)
                     (suggestion condition)))))

(define-condition incompatible-transaction-setting (postgres-json-database-error)
  ((transaction-name :initarg :transaction-name :reader transaction-name)
   (original :initarg :original :reader original)
   (current :initarg :current :reader current))
  (:report (lambda (condition stream)
             (format stream "You cannot embed the transaction for ~A~%of type ~A inside one of ~A."
                     (transaction-name condition)
                     (current condition)
                     (original condition)))))
