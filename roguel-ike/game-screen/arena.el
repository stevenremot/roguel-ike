;;; arena.el --- Arena game screen

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

;;; Code:
(require 'roguel-ike/game-screen/fight)
(require 'roguel-ike/level/factory/string)

(defvar rlk--arenas
  '(("##############################"
      "#............................#"
      "#......#......#......#.......#"
      "#............................#"
      "#............................#"
      "#......#......#......#.......#"
      "#............................#"
      "#............................#"
      "#......#......#......#.......#"
      "#............................#"
      "#............................#"
      "#......#......#......#.......#"
      "#............................#"
      "#............................#"
      "#......#......#......#.......#"
      "#............................#"
      "#............................#"
      "#......#......#......#.......#"
      "#............................#"
      "##############################")

    ("##############################"
     "#..........#####.............#"
     "#..........#####.............#"
     "#..........#####.............#"
     "#............................#"
     "#..........#####.............#"
     "#..........#####.............#"
     "#..........#####.............#"
     "#####.################.#######"
     "#####.################.#######"
     "#####.################.#######"
     "#..........#####.............#"
     "#..........#####.............#"
     "#..........#####.............#"
     "#............................#"
     "#..........#####.............#"
     "#..........#####.............#"
     "#..........#####.............#"
     "##############################")

    ("##############################"
     "#............................#"
     "#............................#"
     "#............................#"
     "#............................#"
     "#............................#"
     "#............................#"
     "#............................#"
     "#.............#..............#"
     "#............###.............#"
     "#...........#####............#"
     "#............###.............#"
     "#.............#..............#"
     "#............................#"
     "#............................#"
     "#............................#"
     "#............................#"
     "#............................#"
     "#............................#"
     "##############################"))
  "Arena's levels.")

(defclass rlk--game-screen-arena (rlk--game-screen-fight)
  ()
  "Game screen for arena mode.")

(defmethod create-level ((self rlk--game-screen-arena))
  "Choose a level in arena's predefined ones."
  (rlk--level-create-from-string-list (nth (random (length rlk--arenas))
                                           rlk--arenas)))

(defmethod setup-level ((self rlk--game-screen-arena))
  "Set all the level's elements."
  (let* ((controller (get-controller self))
         (game (get-game controller))
         (hero (get-hero game))
         (level (get-current-level game))
         (available-positions '()))
    (dotimes (x (width level))
      (dotimes (y (height level))
        (when (is-accessible-p (get-cell-at level x y))
          (add-to-list 'available-positions (cons x y)))))
    (set-level hero level)

    (let ((hero-position (nth (random (length available-positions))
                              available-positions)))
      (set-pos hero (car hero-position) (cdr hero-position)))))

(provide 'roguel-ike/game-screen/arena)

;;; arena.el ends here
