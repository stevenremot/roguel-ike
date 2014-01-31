;;; game-screen.el --- Base class for the different game screens

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
;;

;;; Code:
(require 'roguel-ike/buffer-manager)

(defvar-local rlk--local-game-screen nil
  "The game screen associated to this buffer.")

(defclass rlk--game-screen ()
  ((buffer-manager :initarg :buffer-manager
                   :type rlk--buffer-manager
                   :reader get-buffer-manager
                   :protection :protected
                   :documentation "The game's buffer manager.")
   (end-callback :initarg :end-callback
                 :type function
                 :reader get-end-callback
                 :protection :protected
                 :documentation "A function that must be called at the end of the screen.

This function takes as argument the class' name of the next screen to call,
and an arbitrary number of arguments that will be passed to the setup method
of the next screen."))
  "Base class for game screens.

A game screen is for example a menu screen, or a special game mode."
  :abstract t)

(defmethod setup ((self rlk--game-screen) &rest args)
  "Starts the game screen. THis method must be overriden."
  (error "The method setup must be overriden"))

(defmethod call-end-callback ((self rlk--game-screen) next-screen &rest args)
  "Call the end callback for NEXT-SCREEN, populating setup with ARGS."
  (apply (get-end-callback self) next-screen args))

(defmethod register-in-buffers ((self rlk--game-screen) &rest buffers)
  "Register the game screen in the buffers."
  (dolist (buffer buffers)
    (with-current-buffer buffer
      (setq rlk--local-game-screen self))))

(defmethod quit-game ((self rlk--game-screen))
  (kill-buffers (get-buffer-manager self))
  (call-end-callback self nil))

(provide 'roguel-ike/game-screen)

;;; game-screen.el ends here
