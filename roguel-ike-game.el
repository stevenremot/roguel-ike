;;; roguel-ike-game.el --- Game state

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

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-entity)
(require 'roguel-ike-buffer)

(defclass rlk--game ()
  ((current-level :initarg :level
                  :type rlk--level
                  :reader get-current-level
                  :writer set-current-level
                  :protection :private
                  :documentation "Current level to display.")
   (hero :initarg :hero
         :type rlk--entity-hero
         :reader get-hero
         :protection :private
         :documentation "Player's character.")
   (buffer-manager :initarg :buffer-manager
                   :type rlk--buffer-manager
                   :reader get-buffer-manager
                   :protection :private
                   :documentation "Game's buffer manager."))
  "Contain the game state.")

(provide 'roguel-ike-game)

;;; roguel-ike-game.el ends here
