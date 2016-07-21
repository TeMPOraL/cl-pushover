;;;; package.lisp

(defpackage #:cl-pushover
  (:use #:cl
        #:alexandria)

  (:export #:*pushover-api-endpoint-url*
           #:*default-app-token*
           
           #:+priority-lowest+
           #:+priority-low+
           #:+priority-normal+
           #:+priority-high+
           #:+priority-emergency+
           
           #:+min-retry+
           #:+max-expire+

           #:send-pushover))
