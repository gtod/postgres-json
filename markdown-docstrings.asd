(asdf:defsystem markdown-docstrings
  :author "Gregory Tod <lisp@gtod.net>"
  :version "0.2.0"
  :license "MIT"
  :description "Horrible little docstrings to Markdown converter"
  :depends-on (#:alexandria
               #:uiop
               #:cl-ppcre)
  :components ((:file "markdown-docstrings")))
