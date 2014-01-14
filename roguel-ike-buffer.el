;;; roguel-ike-buffer.el --- roguel-ike buffer management

;;; Commentary:

;; Roguel-ike buffer management system

;;; Code:

(require 'eieio)

(defclass rlk--buffer-manager ()
  ((game-buffer :type buffer
                :protection :private
                :documentation "Buffer in which level is displayed.")
   (message-buffer :type buffer
                   :protection :private
                   :documentation "Buffer in which game messages are displayed."))
  "Buffer management system.
In charge of buffers instanciation and layout organization.")

(defmethod get-game-buffer ((manager rlk--buffer-manager))
  "Return the game buffer.
Create a new one if not set yet."
  (unless (and (slot-boundp manager 'game-buffer)
               (buffer-live-p (oref manager game-buffer)))
    (oset manager game-buffer (get-buffer-create "*rlk-game*")))
  (oref manager game-buffer))

(defmethod get-message-buffer ((manager rlk--buffer-manager))
  "Return the message buffer.
Create anew oen if not set yet."
  (unless (and (slot-boundp manager 'message-buffer)
               (buffer-live-p (oref manager message-buffer)))
    (oset manager message-buffer (get-buffer-create "*rlk-messages*")))
  (oref manager message-buffer))

(defmethod setup-layout ((manager rlk--buffer-manager))
  "Organize buffers in a layout."
  (let
      ((game-buffer (get-game-buffer manager))
       (message-buffer (get-message-buffer manager)))
  (delete-other-windows)
  (display-buffer-same-window game-buffer '())
  (display-buffer-in-side-window message-buffer '((side . right)))))

(provide 'roguel-ike-buffer)

;;; roguel-ike-buffer.el ends here
