;;; message-logger.el --- Message system

;; Copyright (C) 2014 Steven Rémot

;;; Author: Steven Rémot

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(defmethod display-message ((self rlk--message-logger) message &rest format-arguments)
  "Display a message in the message buffer

MESSAGE and FORMAT-ARGUMENTS are passed to the `format' function."
  (with-current-buffer (get-message-buffer self)
    (goto-char (point-min))
    (insert (concat (apply 'format message format-arguments) "\n"))))

(defmethod clear ((self rlk--message-logger))
  "Erase all the previous messages."
  (with-current-buffer (get-message-buffer self)
    (erase-buffer)))

(provide 'roguel-ike/message-logger)

;;; message-logger.el ends here
