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
   :database-safety-net
   :really-do-it
   :with-model-transaction
   :with-db-schema
   :create-db-sequence
   :create-backend
   :backend-exists-p
   :create-model
   :model-exists-p
   :drop-backend!
   :drop-model!
   :flush-prepared-queries
   :make-model-parameters)

  ;; Model user interface
  (:export
   :insert
   :update
   :get
   :all
   :delete
   :delete-all
   :count
   :keys))

;;; Also some convenience functions (not exported) that you may wish to import:
;;; (:import-from :postgres-json :obj :pp-json)
