(in-package #:cl-pushover)

(defparameter *pushover-api-endpoint-url* "https://api.pushover.net/1/messages.json")
(defparameter *default-app-token* nil)

(define-constant +priority-lowest+ -2)
(define-constant +priority-low+ -1)
(define-constant +priority-normal+ 0)
(define-constant +priority-high+ 1)
(define-constant +priority-emergency+ 2)

(define-constant +min-retry+ 30)
(define-constant +max-expire+ 86400)

(define-condition required-argument-missing (error)
  ()
  (:report (lambda (condition stream)
             (format stream "One or more required arguments missing or nil."))))

(deftype pushover-priority () `(integer ,+priority-lowest+ ,+priority-emergency+))

(defun send-pushover (&key destination-key title message (token *default-app-token*) device url-title url (priority +priority-normal+) timestamp sound html callback retry expire)
  "Send a Pushover using your app's `TOKEN' to user/group identified by `DESTINATION-KEY', containing `MESSAGE'.

Optional parameters:
- `TITLE' - message title.
- `DEVICE' - a name of target device, or a string containing comma-separated list of devices. Also accepts a list of strings, which will be concatenated using comma as a separator.
- `URL' - a supplementary URL for the message.
- `URL-TITLE' - a title for the supplementary URL to be displayed instead of the URL on the recipent's device.
- `PRIORITY' - use one of the +PRIORITY- constants.
- `TIMESTAMP' - UNIX timestamp to be used as message's date and time, instead of the time Pushover servers received it.
- `SOUND' - a string name of a sound to be played on recipent's device. Refer to Pushover API details for a list of possible values.
- `HTML' - set to `T' if message is formatted as HTML. Only some tags are supported; refer to Pushover API details for a list of allowed tags.

Required parameters for messages sent with `+PRIORITY-EMERGENCY+' priority:
- `RETRY' - how often (in seconds) Pushover should resend the same notification. Minimum value - 30 seconds.
- `EXPIRE' - for how many seconds the message will continue to be retried unless acknowledged beforehand. Maximum value - 86400 seconds (24 hours).

Optional parameters available for messages sent with `+PRIORITY-EMERGENCY+' priority:
- `CALLBACK' - an URL for messages that Pushover servers will submit a POST request to when the message was acknowledged by recipent."

  ;; rudimentary parameter validation
  (unless (and token
               destination-key
               message)
    (error 'required-argument-missing))
  
  (check-type priority pushover-priority "a valid Pushover message priority value")
  (when (= priority +priority-emergency+)
    (check-type retry (integer #.+min-retry+ *) "a valid Pushover retry time value")
    (check-type expire (integer 0 #.+max-expire+) "a valid Pushover expire time value"))

  ;; actual sending
  (%send-pushover destination-key title message token device url-title url priority timestamp sound html callback retry expire))

(defun %send-pushover (destination-key title message token device url-title url priority timestamp sound html callback retry expire)
  "Sends the actual Pushover message request. Does not do any kind of type/sanity checking on parameters.
Refer to `SEND-PUSHOVER' documentation for meaning of parameters."
  (let ((params (remove-if #'null
                           (mapcar (lambda (name value)
                                     (when value
                                       (cons name (format nil "~A" value))))
                                   (list "token" "user" "message" "title" "device" "url" "url_title" "priority" "timestamp" "sound" "html" "callback" "retry" "expire")
                                   (list token destination-key message title device url url-title priority timestamp sound (when html 1) callback retry expire)))))
    (drakma:http-request *pushover-api-endpoint-url*
                         :method :post
                         :external-format-out :UTF-8
                         :parameters params
                         :content "hack"
                         :content-length 4)))


;;; TODO call for querying receipts

;;; TODO call for canceling emergency-priority retries

;;; TODO call for user/group verification



;;; TODO Subscriptions API (maybe in another file)



;;; TODO Groups API (maybe in another file)



;;; TODO Licensing API (maybe in another file)



;;; TODO Open Client API (maybe in another file, or a separate lib altogether)

