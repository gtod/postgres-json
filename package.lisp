(defpackage :postgres-json
  (:use :cl :alexandria :postmodern :s-sql)
  (:shadow :get :delete :count :sequence)

  ;; Specials
  (:export
   :*pgj-schema*
   :*to-json*
   :*from-json*
   :*db-handle-serialization-failure-p*
   :*serialization-failure-sleep-times*)

  ;; Setup a model
  (:export
   :with-db-schema
   :create-db-sequence
   :create-backend
   :create-model
   :drop-backend!
   :drop-model!
   :flush-prepared-queries
   :make-model-parameters)

  ;; Model user interface
  (:export
   :insert
   :update
   :get
   :delete
   :keys))

;;; Also some convenience functions (not exported) that you may wish to import:
;;; (:import-from :postgres-json :obj :pp-json)
