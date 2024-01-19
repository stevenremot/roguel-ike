;;; test.el --- A test screen with a little level

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
(require 'cl-generic)
(require 'roguel-ike/game-screen/fight)
(require 'roguel-ike/interactive-object/door)
(require 'roguel-ike/entity)
(require 'roguel-ike/behaviour/ai)
(require 'roguel-ike/level/factory/string)


(defclass rlk--game-screen-test (rlk--game-screen-fight)
  ()
  "A test screen that creates a little level.")

(cl-defmethod create-level ((self rlk--game-screen-test))
  "Create the level."
  (let* ((layout '("############"
                   "#..#####...#"
                   "#....#...###"
                   "###......###"
                   "############")))
    (rlk--level-create-from-string-list layout)))

(cl-defmethod setup-level ((self rlk--game-screen-test))
  "Set all the level's elements."
  (let* ((controller (get-controller self))
         (game (get-game controller))
         (hero (get-hero game))
         (level (create-level self))
         (rat (rlk--entity-create-new :rat
                                      (rlk--behaviour-ai "AI behaviour")))
         (door (rlk--interactive-object-door "Door"))
         (message-logger (get-message-logger self)))
    (set-level hero level)
    (set-pos hero 1 1)
    (set-level rat level)
    (set-pos rat 9 1)

    (add-entity level hero)
    (add-entity level rat)

    (set-message-logger hero message-logger)
    (set-message-logger rat message-logger)

    (add-object (get-cell-at level 5 3) door)

    (register (get-dispatcher rat) :died (apply-partially 'win self))
    (set-target (get-behaviour rat) hero)))

(provide 'roguel-ike/game-screen/test)

;;; test.el ends here
