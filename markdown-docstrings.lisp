;;;; This is gross, broken and inflexible and has been done better a
;;;; hundered times before but it does make decent looking API doco,
;;;; in the same order as my package exports and with nice sub heading
;;;; links...  And because it's markdown, I can link to specific API
;;;; functions in other documents like the README and User's Guide.

(defpackage :markdown-docstrings
  (:use #:cl #:alexandria #:cl-ppcre)
  (:export #:generate))

(in-package :markdown-docstrings)

(defparameter *lambda-junk* '(t nil &key &optional &rest &body))

(defvar *doc-cache*)
(defvar *doc-package*)

;; We want to print just the exported symbols in PACKAGE-FILE.  So
;; first we cache the output for every function and special we can
;; find in all the lisp files in the source directory of SYSTEM, a
;; keyword, and only then do we iterate over the exported symbols and
;; write the Markdown to DESTINATION.  PACKAGE must be a package
;; designator for the package where the all the functions and specials
;; we find are interned.  It's a straight-jacket, I know.

(defun generate (&key system (package system) (package-file "package.lisp")
                      (destination (asdf:system-relative-pathname system "doc/api.md")))
  (let ((*doc-package* package)
        (*doc-cache* (make-hash-table :test #'equal))
        (*print-right-margin* 1000)
        (package-file (asdf:system-relative-pathname system package-file)))
    (uiop/filesystem:collect-sub*directories (asdf:system-source-directory system)
                                             #'constantly #'constantly #'per-directory)
    (with-output-to-file (out destination :if-does-not-exist :create
                                          :if-exists :overwrite)
      (with-input-from-file (stream package-file)
        (write-line "# Postgres-JSON Interface" out)
        ;; We assume defpackage form is first
        (let ((defpackage-form (read-with-comments stream)))
          (dolist (form defpackage-form)
            (when (stringp form)
              (let ((heading (string-left-trim "; " form)))
                (format out "* [~A](#~A)~%" heading (substitute #\- #\Space (string-downcase heading))))))
          (format out "~%---~%")
          (dolist (form defpackage-form)
            (when (stringp form)
              (format out "## ~A~%" (string-left-trim "; " form)))
            (when (and (consp form) (eq :export (car form)))
              (dolist (symbol (cdr form))
                (multiple-value-bind (text present-p) (gethash (symbol-name symbol) *doc-cache*)
                  (when present-p
                    (princ text out))))
              (format out "~%---~%"))))))))

(defun read-with-comments (stream)
  (flet ((comment-reader (stream char)
           (declare (ignore char))
           (read-line stream nil #\Newline t)))
    (let ((*readtable* (copy-readtable)))
      (set-macro-character #\; #'comment-reader)
      (read stream))))

(defun walk-tree (fun tree)
  "Walk TREE and call FUN at each node.  Thanks to Lisp Tips."
  (subst-if t (constantly nil) tree :key fun))

(defun per-directory (dir)
  (dolist (file (uiop/filesystem:directory-files dir))
    (when (scan "\\.lisp$" (file-namestring file))
      (with-input-from-file (stream file)
        (handler-case (loop (handle-top-form (read stream)))
          (end-of-file ()
            t))))))

(defun write-inverted (form)
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :invert)
    (write form)))

(defun markdown-escape (string)
  (regex-replace-all "\\*" string "\\\\*"))

(defun handle-top-form (form)
  (when (consp form)
    (let ((head (car form)))
      (case head
        ((cl:defvar cl:defparameter)
         (destructuring-bind (name &optional (value nil value-supplied-p) doc) (rest form)
           (setf (gethash (symbol-name name) *doc-cache*)
                 (with-output-to-string (*standard-output*)
                   (format t "#### ~A~%" (markdown-escape (string-downcase name)))
                   (format t "*~A*~%" head)
                   (terpri)
                   (write-line "```common-lisp")
                   (when value-supplied-p
                     (write-inverted value))
                   (terpri)
                   (write-line "```")
                   (terpri)
                   (format t "~A~%~%" (markdown-escape doc))))))
        ((cl:defclass)
         (let ((name (cadr form)))
           (when-let (docstring (documentation (find-symbol (symbol-name name) *doc-package*) 'type))
             (setf (gethash (symbol-name name) *doc-cache*)
                   (with-output-to-string (*standard-output*)
                     (format t "#### ~A~%" (markdown-escape (string-downcase name)))
                     (format t "*~A*~%" head)
                     (terpri)
                     (format t "~A~%~%" (markdown-escape docstring)))))))
        ((cl:defun cl:defmacro)
         (destructuring-bind (name lambda-list &rest forms) (rest form)
           ;; We *could* get the docstring directly from FORMS now but DOCUMENTATION is easier...
           (declare (ignore forms))
           (when-let (docstring (documentation (find-symbol (symbol-name name) *doc-package*) 'function))
             (setf (gethash (symbol-name name) *doc-cache*)
                   (with-output-to-string (*standard-output*)
                     (format t "#### ~A~%" (string-downcase name))
                     (format t "*~A*~%" (ecase head (cl:defun "Function") (cl:defmacro "Macro")))
                     (terpri)
                     (when lambda-list
                       (write-line "```common-lisp")
                       (dolist (form lambda-list)
                         (write-inverted form)
                         (write-char #\Space))
                       (terpri)
                       (write-line "```")
                       (terpri))
                     ;; Pad the docstring so regex below always works
                     (let ((docstring (format nil " ~A " (markdown-escape docstring)))
                           (symbols (list name)))
                       (walk-tree (lambda (node)
                                    (when (and (symbolp node) (not (member node *lambda-junk*)))
                                      (pushnew node symbols)))
                                  lambda-list)
                       (dolist (symbol (sort symbols (lambda (a b )
                                                       (> (length (symbol-name a))
                                                          (length (symbol-name b))))))
                         (labels ((node-name (node)
                                    (string-upcase (symbol-name node)))
                                  (regex (node)
                                    (format nil "([\\s.,'\"])(~A)([\\s.,'\"])"
                                            (markdown-escape (node-name node)))))
                           (setf docstring (regex-replace-all (regex symbol)
                                                              docstring "\\1**\\2**\\3"))))
                       (format t "~A~%~%" (string-trim " " docstring))))))))))))
