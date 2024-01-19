;;; game.el --- Game state

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
;; Contains game state class

;;; Code:

(require 'cl-generic)
(require 'eieio)
(require 'roguel-ike/level)
(require 'roguel-ike/entity)
(require 'roguel-ike/buffer-manager)

(defclass rlk--game ()
  ((hero :initarg :hero
         :type rlk--entity
         :reader get-hero
         :protection :private
         :documentation "Player's character.")
   (buffer-manager :initarg :buffer-manager
                   :type rlk--buffer-manager
                   :reader get-buffer-manager
                   :protection :private
                   :documentation "Game's buffer manager."))
  "Contain the game state.")

(cl-defmethod get-current-level ((self rlk--game))
  "Return the current level of the game."
  (get-level (get-hero self)))

(provide 'roguel-ike/game)

;;; game.el ends here
