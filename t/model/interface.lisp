(in-package :postgres-json-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:system-registered-p :fset)
    (pushnew :fset *features*)))

#-fset
(test insert0
  (let ((objects (list "123" #(1 2 3) #("He" "Ho" "Har") (obj "foo" "bar" "bing" 17))))
    (with-temp-model (foo (pgj-model))
      (dolist (object objects)
        (let ((key (insert foo object)))
          (is (equalp object (fetch foo key)))))
      (let ((key (insert foo '(1 2 3))))
        (is (equalp '(1 2 3) (coerce (fetch foo key) 'list))))))
  (with-temp-model (foo (pgj-model))
    (with-model-transaction ()
      (dotimes (i 100)
        (insert foo i)))
    (is (= 100 (tally foo)))
    (is (= 4950 (reduce #'+ (fetch-all foo))))))

#-fset
(test supersede0
  (with-temp-model (foo (pgj-model))
    (let ((key (insert foo 123)))
      (supersede foo key 124)
      (is (= 124 (fetch foo key)))
      (supersede foo key (obj "foo" "bar"))
      (is (equalp (obj "foo" "bar") (fetch foo key))))))

(test supersede-history
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

(test fetch0
  (with-temp-model (foo (pgj-object-model))
    (is (null (fetch foo 66)))
    (let ((key (insert foo "123")))
      (is (string= "123" (fetch foo key))))))

(test fetch-all0
  (with-temp-model (foo (pgj-object-model))
    (is (zerop (len (fetch-all foo))))
    (insert foo "123")
    (is (= 1 (len (fetch-all foo))))
    (let ((key (insert foo "124")))
      (is (= 2 (len (fetch-all foo))))
      (excise foo key)
      (is (= 1 (len (fetch-all foo)))))
    (excise-all foo)
    (is (= 0 (len (fetch-all foo))))))

(test excise0
  (with-temp-model (foo (pgj-model))
    (is (null (excise foo 33)))
    (with-model-transaction ()
      (dotimes (i 10)
        (insert foo i)))
    (is (null (excise foo -1)))
    (excise foo (arb-key foo))
    (is (= 9 (tally foo)))
    (excise foo (arb-key foo))
    (is (= 8 (tally foo)))
    (loop until (zerop (tally foo))
          do (let ((key (arb-key foo)))
               (is (= key (excise foo key)))))))

(test excise-all0
  (with-temp-model (foo (pgj-model))
    (is (zerop (excise-all foo)))
    (insert foo "Hi")
    (is (= 1 (excise-all foo)))
    (with-model-transaction ()
      (dotimes (i 10)
        (insert foo (princ-to-string i))))
    (is (= 10 (excise-all foo)))
    (is (zerop (excise-all foo)))))

(test keys0
  (with-temp-model (foo (pgj-model))
    (is (zerop (len (keys foo))))
    (insert foo "Hi")
    (is (= 1 (len (keys foo))))
    (with-model-transaction ()
      (dotimes (i 10)
        (insert foo (princ-to-string i))))
    (is (= 11 (len (keys foo))))
    (let ((key (insert foo '(1 2 3))))
      (is (= 12 (len (keys foo))))
      (excise foo key)
      (is (= 11 (len (keys foo)))))
    (excise-all foo)
    (is (zerop (len (keys foo))))))

(test tally0
  (with-temp-model (foo (pgj-model))
    (is (zerop (tally foo)))
    (insert foo "Hi")
    (is (= 1 (tally foo)))
    (with-model-transaction ()
      (dotimes (i 10)
        (insert foo (princ-to-string i))))
    (is (= 11 (tally foo)))
    (let ((key (insert foo '(1 2 3))))
      (is (= 12 (tally foo)))
      (excise foo key)
      (is (= 11 (tally foo))))
    (excise-all foo)
    (is (zerop (tally foo)))))

(test stash0
  (with-temp-model (foo (pgj-model))
    (let ((key (insert foo (obj "name" "Bill"))))
      (is (not (geth "key" (fetch foo key))))))
  (with-temp-model (foo (pgj-object-model))
    (let ((key (insert foo (obj "name" "Bill"))))
      (is (= key (geth "key" (fetch foo key)))))))

(test having-property0
  (with-temp-model (foo (pgj-object-model))
    (with-model-transaction ()
      (dotimes (i 100)
        (insert foo (obj "iam" i (if (zerop (random 2)) "baz" "bot") 99))))
    (is (zerop (len (having-property foo "no"))))
    (is (= 100 (len (having-property foo "iam"))))
    (is (plusp (len (having-property foo "baz"))))
    (is (plusp (len (having-property foo "bot"))))
    (is (= 100 (+ (len (having-property foo "baz"))
                  (len (having-property foo "bot")))))))

(defun cl-symbol-inserts (model)
  (loop for sym being each external-symbol of :cl
        for str = (symbol-name sym)
        for abbrev = (subseq str 0 (min (length str) 2))
        do (insert model (obj "abbrev" abbrev))
        collect abbrev))

#-fset
(test enumerate-property0
  (with-temp-model (foo (pgj-object-model))
    (let ((abbrevs (with-model-transaction () (cl-symbol-inserts foo))))
      (is (equal (sort (delete-duplicates abbrevs :test #'string=) #'string<)
                 (sort (enumerate-property foo "abbrev") #'string<))))))

(test filter0
  (with-temp-model (foo (pgj-object-model))
    (with-model-transaction ()
      (cl-symbol-inserts foo))
    (is (= 2 (len (contains foo (obj "abbrev" "AB")))))
    (is (= 3 (len (contains foo (obj "abbrev" "RE") :limit 3))))
    #-fset
    (let ((results (contains foo (obj "abbrev" "RE") :properties '("abbrev"))))
      (is (= 1 (hash-table-count (first results)))))))
