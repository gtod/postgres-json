;;; My own preference is to import :postgres-json-model as well, but
;;; the model functions appear here qualified by their nickname, such
;;; as pj:get, for clarity.

(defpackage :human
  (:use :cl :postgres-json :alexandria))

(in-package :human)

;;;; Instructions

;;; Compile this file
;;; Start with these three if you haven't already:
;;; (pomo:connect-toplevel "<db>" "<user>" "" "localhost")
;;; (pomo:set-search-path *default-search-path*)
;;; (create-backend)

;;; Now try (create) then (run-1) and (run-2)

;;; Caveat: the point is to demonstrate the model interface rather
;;; than write the most efficient or extensible code.

(defparameter *human-url* "http://gtod.github.io/human.json")
(defparameter *human-file* "/tmp/postgres-json-human.json")

(defparameter *gifts* '("Gold" "Book" "Patience" "Goat"))

;;;; Implementation

;;; Util

(defmacro show (form)
  `(progn
     (print ',form)
     (pp-json ,form)))

(defmacro g (key hash)
  `(gethash ,key ,hash))

(defmacro with-keys ((&rest pairs) object &body body)
  (once-only (object)
    `(symbol-macrolet (,@(loop for pair in pairs
                               collect `(,(car pair) (g ,(cadr pair) ,object))))
       ,@body)))

;;; Model

(defun random-human ()
  (let ((keys (pj:keys 'human)))
    (let ((key (elt keys (random (pj:count 'human)))))
      (pj:get 'human key))))

(defun random-gift-type ()
  (elt *gifts* (random (length *gifts*))))

(defun distribute-gifts ()
  (with-model-transaction ()
    (dotimes (i (round (* (pj:count 'human) 9/10)) (pj:count 'gift))
      (let ((human (random-human)))
        (let ((gift (obj "human-key" (g "key" human)
                         "type" (random-gift-type)
                         "quantity" (1+ (random 10)))))
          (pj:insert 'gift gift))))))

;;;; Interface

(defun create ()
  (create-model 'human)
  (create-model 'gift)

  (unless (probe-file *human-file*)
    (ql-http:fetch *human-url* *human-file*))

  ;; Ahhh, we need empty arrays to go in as emptry arrays!
  (with-input-from-file (stream *human-file*)
    (with-model-transaction ()
      (loop for human across (yason:parse stream :json-arrays-as-vectors t)
            do (pj:insert 'human human)))))

;;; Model functions that lightly wrap the SQL

(defun run-1 ()
  (show (length (pj:filter 'human :contain (obj "gender" "female"))))

  (let ((keys (pj:keys 'human)))
    (let ((id (elt keys (random (pj:count 'human)))))
      (show (g "name" (pj:get 'human id)))))

  (let ((human (show (first (pj:filter 'human :contain (obj "name" "Marcella Marquez"))))))
    (with-keys ((key "key") (friends "friends")) human
      (push (obj "name" "Horace Morris") friends)
      (show (pj:update 'human key human))
      (show (length (g "friends" (pj:get 'human key))))
      (show (length (pj:history 'human key)))))

  (show (pj:filter 'human :keys '("age" "tags") :contain (obj "tags" '("ut" "labore"))))
  (show (length (pj:filter 'human :contain (obj "isActive" t "age" 21))))

  (show (length (pj:exists 'human "eyeColor"))))

;;;; User defined SQL queries for when FILTER does not cut it.

;;; These must return JSON objects only.
;;; Macroexpand away to see the generated SQL.

;;; Try slime-edit-definition (M-.) on DEFINE-JSON-QUERY to read
;;; details on the syntactic sugar of j->, j->> and jbuild and named
;;; parameter interpolation.

(define-json-query rich-humans$ (min-balance gender)
  (:order-by

   ;; jbuild makes a new JSON object with just the listed top level keys
   (:select (jbuild ("key" "guid" "gender" "name" "balance"))

    :from 'human ;; this is why we need to set the search path above

    ;; j->> is a little syntactic sugar to get at the top levels keys in 'jdoc
    ;; j->> returns text, so we must cast to Postgres number type for comparisons
    :where (:and (:>= (:type (j->> "balance") real) min-balance)
                 (:= (j->> "gender") gender))) ;; can also do this using 'containment', see below

   (:type (j->> "balance") real)))

;; We need filter arg as a JSON string, so request a funcall on *to-json* at run time
(define-json-query one-friend-humans$ ((*to-json* filter) email-regex)
  (:select 'jdoc ;; 'jdoc is the generic name for the JSON column in all models
   :from 'human

   ;; Our 'jdoc column is Postgres type jsonb so j-> returns
   ;; Postgres jsonb, thus we apply jsonb ops to it...
   :where (:and (:or (:@> 'jdoc filter))                     ;; Postgres json 'containment' op
                (:= (:jsonb-array-length (j-> "friends")) 1) ;; Postgres jsonb function
                (:~ (j->> "email") email-regex))))           ;; Postgres regex function

;; Example of a join
(define-json-query uncharitable-humans$ ()
  (:select (jbuild (human "name") (gift "type" "quantity"))
   :from 'human
   :inner-join 'gift
   :on (:= (j-> human "key") (j-> gift "human-key"))

   ;; We could also write the below :where clause as
   ;; :where (:= (:type (j->> gift "quantity") int4) 1)

   ;; j->> asks that gift "quantity" be converted to Postgres type
   ;; text, which we then cast to Postgres type int4 to compare with 1.

   ;; What we actually do below do is convert Postgres type integer 1
   ;; to Postgres type json which (sadly) we must then explicitly cast
   ;; to Postgres type jsonb before comparing with key "quantity".
   ;; Postgres operator -> gives the raw json (or jsonb) of the key
   ;; while Postgres operator ->> converts it to text.  This library
   ;; only uses Postgres type jsonb: see the Postgres docs at
   ;; http://www.postgresql.org/docs/9.4/static/functions-json.html
   :where (:= (j-> gift "quantity") (:type (:to-json 1) jsonb))))

(defun drop ()
  (drop-model! 'human)
  (drop-model! 'gift))

;; Use the latatude info...
;; Make connection and set search path immediatley after
;; Foreign keys?  Promote to foreign key?
;; Domain consolidation? (enumerate 'human "eyeColor")
