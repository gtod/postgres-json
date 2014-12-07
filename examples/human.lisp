;;; My own preference is to import :postgres-json-model as well, but
;;; the model functions appear here qualified by their nickname, such
;;; as pj:get, for clarity.

(defpackage :human
  (:use :cl :postgres-json :alexandria))

(in-package :human)

;;;; Instructions

;;; 1. Set this for your Postgres 9.4 DB
(defparameter *connection* '("cusoon" "gtod" "" "localhost" :port 5433))

;;; 2. Compile this file (M-x slime-compile-file in emacs)
;;; 3. Call (create-backend) if you have not done that elsewhere
;;; 4. Now review the code of, run, and consider the output of
;;;    CREATE, LOAD-HUMANS, MODEL-TEST and QUERY-TEST in turn.

;;; Caveat: the point is to demonstrate the model interface rather
;;; than write the most efficient or extensible code.

(defparameter *human-url* "http://gtod.github.io/human.json")
(defparameter *human-file* "/tmp/postgres-json-human.json")

(defparameter *gifts* '("Gold" "Book" "Patience" "Goat"))

;;;; Implementation

;;; Util

(defmacro with-pj-conn (() &body body)
  `(pomo:with-connection *connection*
     ;; There is a small overhead for this, see (alter-role-set-search-path)
     ;; for a more permanent solution.
     (pomo:set-search-path *default-search-path*)
     ,@body))

(defmacro show (form)
  `(progn
     (print ',form)
     (pp-json ,form)))

(defmacro with-keys ((&rest pairs) object &body body)
  (once-only (object)
    `(symbol-macrolet (,@(loop for pair in pairs
                               collect `(,(car pair) (gethash ,(cadr pair) ,object))))
       ,@body)))

;;; Model

(defun random-human ()
  (pj:get 'human (elt (pj:keys 'human) (random (pj:count 'human)))))

(defun random-gift-type ()
  (elt *gifts* (random (length *gifts*))))

(defun distribute-gifts ()
  (format t "~%Loading gifts...~%")
  (with-model-transaction ()
    (dotimes (i (round (* (pj:count 'human) 9/10)) (pj:count 'gift))
      (let ((human (random-human)))
        (let ((gift (obj "human-key" (gethash "key" human)
                         "type" (random-gift-type)
                         "quantity" (1+ (random 30)))))
          (pj:insert 'gift gift))))))

;;;; Interface

(defun create ()
  (unless (model-exists-p 'human)
    (with-model-transaction ()
      (create-model 'human)
      (create-model 'gift))))

(defun load-humans ()
  (write-line "Loading humans...")

  (unless (probe-file *human-file*)
    (ql-http:fetch *human-url* *human-file*))

  (with-input-from-file (stream *human-file*)
    (with-model-transaction ()
      ;; We need empty arrays to go in as empty arrays, not empty
      ;; lists (ie. nil) so that :jsonb-array-length can work (see below).
      (loop for human across (yason:parse stream :json-arrays-as-vectors t)
            do (pj:insert 'human human)
            finally (return (pj:count 'human))))))

(defun cleanup ()
  (with-model-transaction ()
    (pj:delete-all 'human)
    (pj:delete-all 'gift)))

(defun drop ()
  (drop-model! 'human)
  (drop-model! 'gift))

;;; Model functions that lightly wrap the SQL

(defun model-test ()
  (with-pj-conn ()
    (show (length (pj:filter 'human :contain (obj "gender" "female"))))

    (show (gethash "name" (random-human)))

    (let ((human (show (first (pj:filter 'human :contain (obj "name" "Marcella Marquez"))))))
      (with-keys ((key "key") (friends "friends")) human
        (push (obj "name" "Horace Morris" "id" (length friends)) friends)
        (show (pj:update 'human key human))
        (show (length (gethash "friends" (pj:get 'human key))))
        (show (length (pj:history 'human key)))))

    (show (pj:filter 'human :keys '("age" "tags") :contain (obj "tags" '("ut" "labore"))))
    (show (length (pj:filter 'human :contain (obj "isActive" t "age" 21))))

    (show (length (pj:exists 'human "eyeColor")))
    (show (pj:distinct 'human "favoriteFruit")))
  (values))

;;;; User defined SQL queries for when FILTER does not cut it.

;;; These must return JSON objects only.
;;; Macroexpand away to see the generated SQL.

;;; Try slime-edit-definition (M-.) on DEFINE-JSON-QUERY to read
;;; details on the syntactic sugar of j->, j->>, jbuild, to-json and
;;; also named parameter interpolation.

(define-json-query rich-humans$ (min-balance gender)
  (:order-by

   ;; jbuild makes a new JSON object with just the listed top level keys
   (:select (jbuild ("key" "guid" "gender" "name" "balance"))

    :from 'human ;; this is why we need to set the search path above
                 ;; otherwise it would need to be 'pgj_model.history

    ;; j->> is a little syntactic sugar to get at the top level keys in 'jdoc
    ;; j->> returns text, so we must cast to Postgres number type for comparisons
    :where (:and (:>= (:type (j->> "balance") real) min-balance)
                 (:= (j->> "gender") gender))) ;; can also do this using 'containment', see below

   (:type (j->> "balance") real)))

;; We need filter arg as a JSON string, so request a funcall on *to-json* at run time
(define-json-query one-friend-humans$ ((*to-json* filter) email-regex)
  (:select 'jdoc ;; 'jdoc is the generic name for the JSON column in all models
   :from 'human

   ;; Our 'jdoc column is Postgres type jsonb so j-> returns
   ;; Postgres jsonb, thus we apply jsonb functions to it...
   :where (:and (:or (:@> 'jdoc filter))                     ;; Postgres json 'containment' op
                (:= (:jsonb-array-length (j-> "friends")) 1) ;; Postgres jsonb function
                (:~ (j->> "email") email-regex))))           ;; Postgres regex function

;; Example of a join
(define-json-query uncharitable-humans$ ()
  (:select (jbuild (human "name") (gift "type" "quantity"))
   :from 'human
   :inner-join 'gift
   :on (:= (j-> human "key") (j-> gift "human-key"))
   :where (:= (j-> gift "quantity") (to-json 1))

   ;; We could also write the above :where clause as
   ;; :where (:= (:type (j->> gift "quantity") int4) 1)
   ;; which is what we did with min-balance in the rich-humans$ query
   ;; above.  j->> asks that gift "quantity" be converted to Postgres
   ;; type text, which we then cast to Postgres type int4 to compare
   ;; with 1.

   ;; What we actually do is convert Postgres type integer 1 to
   ;; Postgres type jsonb (using a little syntactic sugar) before
   ;; comparing it with key "quantity".  Postgres operator -> gives
   ;; the raw jsonb of the key while Postgres operator ->>
   ;; converts it to text.  This library only uses Postgres type
   ;; jsonb. See also the Postgres docs at
   ;; http://www.postgresql.org/docs/9.4/static/functions-json.html
   ))

(defun query-test ()
  (with-pj-conn ()
    (show (rich-humans$ 3900 "male"))
    (show (one-friend-humans$ (obj "gender" "female") "^c"))

    (when (zerop (pj:count 'gift))
      (distribute-gifts))
    (show (uncharitable-humans$)))
  (values))

;; Make connection and set search path immediatley after
;; Foreign keys?  Promote to foreign key?  Yep, can't do in JSON...
;; Domain consolidation? (distinct 'human "eyeColor")
;; Is the nsubst as we walk tree buggy?  Can we not save
;; up all desired subs and do them all together?

;; We could promote to foreign key:
;; What key and Postgres type in your child model?  What master table?
;; We make a column, populate it, add the index
;; For every insert, we grab that named key and stuff it into the FKey col
