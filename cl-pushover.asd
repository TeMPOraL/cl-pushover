;;;; cl-pushover.asd

(asdf:defsystem #:cl-pushover
  :serial t

  :long-name "Common Lisp bindings to Pushover API"
  :author "Jacek Złydach"
  :version "0.0.1"
  :description "A set of Common Lisp bindings to Pushover - a system for sending push notifications to mobile devices."

  :license "TBD"
  :homepage "https://github.com/TeMPOraL/cl-pushover"
  :bug-tracker "https://github.com/TeMPOraL/cl-pushover/issues"
  :source-control (:git "https://github.com/TeMPOraL/cl-pushover.git")
  :mailto "temporal.pl+clpushover@gmail.com"

  :encoding :utf-8
  
  :depends-on (#:alexandria
               #:drakma)
  
  :components ((:file "package")

               (:file "main" :depends-on ("grimoire"))))
