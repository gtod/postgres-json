(in-package :postgres-json-test)

(test update-history
  (with-temp-model (with-history (pgj-history-model))
    (insert with-history (obj "a" "b") 7)
    (supersede with-history 7 (obj "a" "c"))
    (supersede with-history 7 (obj "a" "d"))
    (is (= 2 (len (history with-history 7))))
    (with-model-transaction ()
      (insert with-history "123" 8)
      (supersede with-history 8 "124")
      (supersede with-history 8 "125")
      (is (= 2 (len (history with-history 8)))))))
