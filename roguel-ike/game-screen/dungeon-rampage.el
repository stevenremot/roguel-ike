;;; dungeon-rampage.el --- Dungeon mode

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
;; The arena mode is a fighting mode where the player
;; faces several enemies in one level.  The player must defeat
;; all of them.  The players chooses itself the difficulty level
;; he wants.

;;; Commentary:
;;

;;; Code:
(require 'cl-generic)
(require 'roguel-ike/game-screen/fight)
(require 'roguel-ike/dungeon)
(require 'roguel-ike/level/populator/periodic)

(defclass rlk--game-screen-dungeon-rampage (rlk--game-screen-fight)
  ((dungeon :type rlk--dungeon
            :reader get-dungeon
            :protection :private
            :documentation "The current dungeon."))
  "In this mode, the player must go as far as he can in a dungeon.

Monsters become harder and harder.")

(cl-defmethod initialize-instance :after ((self rlk--game-screen-dungeon-rampage) slots)
  (oset self dungeon (rlk--dungeon)))

(cl-defmethod setup-level ((self rlk--game-screen-dungeon-rampage))
  (let* ((controller (get-controller self))
         (game (get-game controller))
         (hero (get-hero game))
         (message-logger (get-message-logger self))
         (dungeon (get-dungeon self))
         (dispatcher (get-dispatcher dungeon)))
    (set-message-logger hero message-logger)
    (register dispatcher :reached-level (apply-partially 'setup-new-level self))
    (teleport-to-level dungeon 0 :up hero)
    (register dispatcher :changed-level (apply-partially 'run-level self))))

(cl-defmethod run-level ((self rlk--game-screen-dungeon-rampage) level-number)
  "Called when the level changed to run it."
  (do-step (get-time-manager (get-current-level (get-game (get-controller self))))))

(cl-defmethod setup-new-level ((self rlk--game-screen-dungeon-rampage) level-number)
  "Called when a new level is created. Set it up."
  (let* ((hero (get-hero (get-game (get-controller self))))
         (populator (rlk--level-populator-periodic
                     :level (get-level (get-dungeon self) level-number)
                     :hero hero
                     :difficulty level-number
                     :message-logger (get-message-logger self))))
    (dotimes (i 5)
      (spawn-entity populator))
    (oset self base-hero-data (rlk--entity-create-hero-data
                               (get-name (get-base-hero-data self))
                               hero))))

(provide 'roguel-ike/game-screen/dungeon-rampage)

;;; dungeon-rampage.el ends here
