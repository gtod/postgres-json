(in-package :postmodern)

;;; This is the smallest change I could think of and ideally becomes
;;; part of Postmodern proper.  Has current behaviour by default.

;; 1) Define and 2) Export this 3) Export the various call-with functions
(defvar *transaction-mode* "")

(export '(*transaction-mode* call-with-transaction call-with-logical-transaction
          call-with-ensured-transaction))

(defun call-with-transaction (body)
  (let ((transaction (make-instance 'transaction-handle)))
    (execute (format nil "BEGIN ~A" *transaction-mode*)) ; 4) This one line change
    (unwind-protect
         (multiple-value-prog1
             (let ((*transaction-level* (1+ *transaction-level*))
                   (*current-logical-transaction* transaction))
               (funcall body transaction))
           (commit-transaction transaction))
      (abort-transaction transaction))))

;; 5) This is a bug fix
(defmethod commit-logical-transaction ((savepoint savepoint-handle))
  (release-savepoint savepoint))
