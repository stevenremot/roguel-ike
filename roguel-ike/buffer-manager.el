;;; buffer-manager.el --- roguel-ike buffer management

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
;; Roguel-ike buffer management system

;;; Code:

(require 'eieio)

(defclass rlk--buffer-manager ()
  ((game-buffer :type buffer
                :protection :private
                :documentation "Buffer in which level is displayed.")
   (stats-buffer :type buffer
                 :protection :private
                 :documentation "Buffer in which hero statistics are displayed.")
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

(defmethod get-stats-buffer ((manager rlk--buffer-manager))
  "Return the statistics buffer.
Create a new one if not set yet."
  (unless (and (slot-boundp manager 'stats-buffer)
               (buffer-live-p (oref manager stats-buffer)))
    (oset manager stats-buffer (get-buffer-create "*rlk-statistics*")))
  (oref manager stats-buffer))

(defmethod get-message-buffer ((manager rlk--buffer-manager))
  "Return the message buffer.
Create a new one if not set yet."
  (unless (and (slot-boundp manager 'message-buffer)
               (buffer-live-p (oref manager message-buffer)))
    (oset manager message-buffer (get-buffer-create "*rlk-messages*")))
  (oref manager message-buffer))

(defmethod setup-layout ((manager rlk--buffer-manager))
  "Organize buffers in a layout."
  (let
      ((game-buffer (get-game-buffer manager))
       (stats-buffer (get-stats-buffer manager))
       (message-buffer (get-message-buffer manager))
       (game-window (get-buffer-window (current-buffer)))
       (stats-window nil)
       (message-window nil))
  (delete-other-windows game-window)
  (display-buffer-same-window game-buffer '())
  (setq stats-window (split-window game-window nil 'right))
  (select-window stats-window)
  (display-buffer-same-window stats-buffer '())
  (setq message-window (split-window stats-window nil 'below))
  (select-window message-window)
  (display-buffer-same-window message-buffer '())
  (select-window game-window)))

(defmethod kill-buffers ((manager rlk--buffer-manager))
  "Kill buffers."
  (kill-buffer (get-message-buffer manager))
  (kill-buffer (get-stats-buffer manager))
  (kill-buffer (get-game-buffer manager)))

(provide 'roguel-ike/buffer-manager)

;;; buffer-manager.el ends here
