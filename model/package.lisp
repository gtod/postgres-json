;;;; Model user interface

(defpackage :postgres-json-model
  (:nicknames #:pj)
  (:shadowing-import-from #:postgres-json
                          #:get
                          #:delete
                          #:count)
  (:import-from #:postgres-json
                #:insert
                #:update
                #:all
                #:delete-all
                #:keys)
  (:export
   #:get
   #:delete
   #:count

   #:insert
   #:update
   #:all
   #:delete-all
   #:keys))
