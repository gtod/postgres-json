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
                     (suggestion condition))))
  (:documentation "Signaled to prevent accidental deletion of database
assets such as tables or schema."))

(defun really-do-it (condition)
  "Invoke a 'REALLY-DO-IT restart."
  (declare (ignore condition))
  (invoke-restart 'really-do-it))
