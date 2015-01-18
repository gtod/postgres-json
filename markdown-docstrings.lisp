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
                (multiple-value-bind (doc-list present-p) (gethash (symbol-name symbol) *doc-cache*)
                  (when present-p
                    (dolist (text (reverse doc-list))
                      (princ text out)))))
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

(defun class-docstring (class-form)
  (dolist (form (cdddr class-form))
    (when (eq :documentation (car form))
      (return (cadr form)))))

(defun method-qualifier-p (form)
  (and (eq 'cl:defmethod (car form))
       (keywordp (third form))))

(defun declaration-p (form)
  (and (consp form) (eq 'cl:declare (car form))))

(defun def-docstring (def-form)
  (let ((n (if (method-qualifier-p def-form) 4 3)))
    (dolist (form (nthcdr n def-form))
      (unless (or (declaration-p form) (stringp form))
        (return))
      (when (stringp form)
        (return form)))))

(defun def-form-name (head)
  (ecase head
    (cl:defvar "Dynamic variable")
    (cl:defparameter "Dynamic variable")
    (cl:defclass "Class")
    (cl:define-condition "Condition")
    (cl:defun "Function")
    (cl:defgeneric "Generic function")
    (cl:defmethod "Method")
    (cl:defmacro "Macro")))

(defun output-def-form (name head lambda-list docstring)
  (with-output-to-string (*standard-output*)
    (format t "#### ~A~%" (string-downcase name))
    (format t "*~A*~%" (def-form-name head))
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
      (format t "~A~%~%" (string-trim " " docstring)))))

(defun handle-top-form (form)
  (when (consp form)
    (let ((head (car form)))
      (case head
        ((cl:defvar cl:defparameter)
         (destructuring-bind (name &optional (value nil value-supplied-p) doc) (rest form)
           (push (with-output-to-string (*standard-output*)
                   (format t "#### ~A~%" (markdown-escape (string-downcase name)))
                   (format t "*~A*~%" (def-form-name head))
                   (when (and value-supplied-p value)
                     (terpri)
                     (write-line "```common-lisp")
                     (write-inverted value)
                     (terpri)
                     (write-line "```"))
                   (terpri)
                   (format t "~A~%~%" (markdown-escape doc)))
                 (gethash (symbol-name name) *doc-cache* '()))))
        ((cl:defclass cl:define-condition)
         (let ((name (cadr form)))
           (when-let (docstring (class-docstring form))
             (push (with-output-to-string (*standard-output*)
                     (format t "#### ~A~%" (markdown-escape (string-downcase name)))
                     (format t "*~A*~%" (def-form-name head))
                     (terpri)
                     (format t "~A~%~%" (markdown-escape docstring)))
                   (gethash (symbol-name name) *doc-cache* '())))))
        ((cl:defgeneric)
         (let ((name (cadr form)))
           (when-let (docstring (class-docstring form))
             (push (output-def-form name head (third form) docstring)
                   (gethash (symbol-name name) *doc-cache* '())))
           (dolist (form (cdddr form))
             (when (eq :method (car form))
               (when-let ((docstring (def-docstring (cons 'foo form))))
                 (let ((lambda-list (if (keywordp (cadr form)) (third form) (second form))))
                   (push (output-def-form name 'cl:defmethod lambda-list docstring)
                         (gethash (symbol-name name) *doc-cache* '()))))))))
        ((cl:defun cl:defmacro cl:defmethod)
         (destructuring-bind (name lambda-list &rest forms) (rest form)
           (declare (ignore forms))
           (when-let (docstring (def-docstring form))
             (push (output-def-form name head lambda-list docstring)
                   (gethash (symbol-name name) *doc-cache* '())))))))))
