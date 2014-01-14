;;; roguel-ike-message.el --- Message system

;;; Commentary:

;; Displays game messages

;;; Code:

(require 'eieio)

(defclass rlk--message-logger ()
  ((message-buffer :initarg :message-buffer
                   :type buffer
                   :reader get-message-buffer
                   :protection :private
                   :documentation "Buffer in which messages are displayed."))
  "Display game messages in a buffer.")

(defmethod display-message ((logger rlk--message-logger) message)
  "Display a message in the message buffer"
  (with-current-buffer (get-message-buffer logger)
    (goto-char (point-max))
    (insert (concat message "\n"))))

(provide 'roguel-ike-message)

;;; roguel-ike-message.el ends here
