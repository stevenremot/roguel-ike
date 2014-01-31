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
(require 'roguel-ike/game-screen/fight)
(require 'roguel-ike/interactive-object/door)
(require 'roguel-ike/entity)
(require 'roguel-ike/behaviour/ai)


(defclass rlk--game-screen-test (rlk--game-screen-fight)
  ()
  "A test screen that creates a little level.")

(defun rlk--get-cells-from-layout (layout)
  "Create a cell level from a LAYOUT, a list of string representing the level."
  (let ((cells '()))
    (dolist (line layout)
      (let ((cell-line '()))
        (dolist (character (split-string line "" t))
          (setq cell-line
                (append cell-line
                        (list
                         (cond ((string-equal character "#")
                                (rlk--level-cell "Wall cell" :type :wall))
                               ((string-equal character ".")
                                (rlk--level-cell-ground "Ground cell"))
                               (t
                                (rlk--level-cell "Unknown cell" :type :void)))))))
        (setq cells (append cells (list cell-line)))))
    cells))

(defmethod create-level ((self rlk--game-screen-test))
  "Create the level."
  (let* ((layout '("############"
                   "#..#####...#"
                   "#....#...###"
                   "###......###"
                   "############"))
         (cells (rlk--get-cells-from-layout layout)))
    (rlk--level "Level" :cells cells)))

(defmethod setup-level ((self rlk--game-screen-test))
  "Set all the level's elements."
  (let* ((controller (get-controller self))
         (game (get-game controller))
         (hero (get-hero game))
         (level (get-current-level game))
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

    (add-object (get-cell-at level 5 3) door)))

(provide 'roguel-ike/game-screen/test)

;;; test.el ends here
