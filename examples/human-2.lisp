(in-package :pj-human)

;;;; This is the second part of the human JSON example.  You need to
;;;; have setup your Postgres connection and run (create) and
;;;; (load-humans) from human-1.lisp before this will work.

;;;; Simply evaluate (query-test)

;;;; Gift model

(defparameter *gift-types* '("Gold" "Book" "Patience" "Goat"))

(defun random-gift-type ()
  (elt *gift-types* (random (length *gift-types*))))

(defun distribute-gifts ()
  (format t "~%Loading gifts...~%")
  (with-model-transaction ()
    (dotimes (i (tally 'human) (tally 'gift))
      (let ((human (random-human)))
        (let ((gift (obj "human-key" (gethash "key" human)
                         "type" (random-gift-type)
                         "quantity" (1+ (random 30)))))
          (insert 'gift gift))))))

;;;; User defined S-SQL queries

;;; For when FILTER and the other interface functions demonstrated in
;;; human-1 are not sufficient.

;;; These queries must return a single JSON object per result row.
;;; Macroexpand the define-json-query forms and the individual j->,
;;; j->>, jbuild and to-jsonb forms to see the generated S-SQL.  See
;;; also the User Guide on "User defined queries" and "JSON query
;;; syntactic sugar" for details on these queries.

(define-json-query rich-humans$ (min-balance gender)
  (:order-by
   (:select (jbuild ("key" "guid" "gender" "name" "balance"))
    :from 'human
    :where (:and (:>= (:type (j->> "balance") real) min-balance)
                 (:= (j->> "gender") gender)))
   (:type (j->> "balance") real)))

(define-json-query one-friend-humans$ ((*to-json* filter) email-regex)
  (:select 'jdoc
   :from 'human
   :where (:and (:or (:@> 'jdoc filter))
                (:= (:jsonb-array-length (j-> "friends")) 1)
                (:~ (j->> "email") email-regex))))

(define-json-query uncharitable-humans$ ()
  (:select (jbuild (human "name") (gift "type" "quantity"))
   :from 'human
   :inner-join 'gift
   :on (:= (j-> human "key") (j-> gift "human-key"))
   :where (:= (j-> gift "quantity") (to-jsonb 1))))

;;;; Interface

(defun query-test ()
  (with-pj-connection ()
    (show (rich-humans$ 3900 "male"))
    
    (show (one-friend-humans$ (obj "gender" "female") "^c"))

    (when (zerop (tally 'gift))
      (distribute-gifts))
    
    (show (uncharitable-humans$)))
  
  (values))
