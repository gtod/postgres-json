;;;; JSON -> JSOWN objects -> Fset objects

(in-package :postgres-json)

(setf jsown:*parsed-true-value* :true)
(setf jsown:*parsed-false-value* :false)
(setf jsown:*parsed-null-value* :null)

(defun jsown-to-fset (object)
  "Recursively convert arbitrary JSOWN JSON objects to FSET objects."
  (etypecase object
    (real object)
    (string object)
    (null (fset:empty-seq))
    (keyword (ecase object
               (:true t)
               ((:false :null) nil)))
    (list (if (eq :obj (car object))
              (let ((map (fset:empty-map)))
                (dolist (pair (cdr object) map)
                  (destructuring-bind (car . cdr) pair
                    (setq map (fset:with map car (jsown-to-fset cdr))))))
              (gmap:gmap :seq #'jsown-to-fset (:list object))))))

(setf *from-json* (compose #'jsown-to-fset #'jsown:parse))

;; (defun test-parse ()
;;   (jsown-to-fset (jsown:parse "[{}, [], null, true, false, 1, 2, \"three\", { \"foo\":17}]")))
