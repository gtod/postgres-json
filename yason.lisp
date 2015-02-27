;;;; There are published was to do this without mucking with yason
;;;; internals but I could not easily make those work for pretty
;;;; printing with an indent...

(in-package :yason)

(defmethod encode ((object fset:map) &optional (stream *standard-output*))
  (with-aggregate/object (stream #\{ #\})
    (fset:do-map (key value object object)
      (with-element-output ()
        (encode-key/value key value stream)))))

(defmethod encode ((object fset:seq) &optional (stream *standard-output*))
  (with-aggregate/object (stream #\[ #\])
    (fset:do-seq (value object :value object)
      (with-element-output ()
        (encode value stream)))))

(defmethod encode ((object fset:set) &optional (stream *standard-output*))
  (with-aggregate/object (stream #\[ #\])
    (fset:do-set (value object object)
      (with-element-output ()
        (encode value stream)))))
