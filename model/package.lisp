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
                #:get-all
                #:delete-all
                #:contains
                #:exists
                #:keys)
  (:export
   #:get
   #:delete
   #:count

   #:insert
   #:update
   #:get-all
   #:delete-all
   #:contains
   #:exists
   #:keys))
