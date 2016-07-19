(in-package #:cl-pushover)

;;; TODO.

(defparameter *default-app-token* "")

(define-constant +priority-lowest+ -2)
(define-constant +priority-low+ -1)
(define-constant +priority-normal+ 0)
(define-constant +priority-high+ 1)
(define-constant +priority-emergency+ 2)

(define-constant +min-retry+ 30)
(define-constant +max-expire+ 86400)


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

Optional parameters available for messages sent with `+PRIORITY-EMERGENCY+' priority:
- `CALLBACK' - an URL for messages that Pushover servers will submit a POST request to when the message was acknowledged by recipent.
- `RETRY' - how often (in seconds) Pushover should resend the same notification. Minimum value - 30 seconds.
- `EXPIRE' - for how many seconds the message will continue to be retried unless acknowledged beforehand. Maximum value - 86400 seconds (24 hours)."
  
  (unless (and destination-key
               message)
    ;; TODO throw up - required parameters
    )

  ;; TODO implement
  )


;;; TODO call for querying receipts

;;; TODO call for canceling emergency-priority retries

;;; TODO call for user/group verification



;;; TODO Subscriptions API (maybe in another file)



;;; TODO Groups API (maybe in another file)



;;; TODO Licensing API (maybe in another file)



;;; TODO Open Client API (maybe in another file, or a separate lib altogether)

