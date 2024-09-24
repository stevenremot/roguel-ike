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
(require 'cl-generic)
(require 'roguel-ike/game-screen/fight)
(require 'roguel-ike/level/factory/string)
(require 'roguel-ike/level/populator/arena)

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
  ((enemy-count :type integer
                :protection :private
                :documentation "The number of remaining enemies."))
  "Game screen for arena mode.")

(cl-defmethod create-level ((self rlk--game-screen-arena))
  "Choose a level in arena's predefined ones."
  (rlk--level-create-from-string-list (nth (random (length rlk--arenas))
                                           rlk--arenas)))

(cl-defmethod setup-level ((self rlk--game-screen-arena))
  "Set all the level's elements."
  (let* ((controller (get-controller self))
         (game (get-game controller))
         (hero (get-hero game))
         (level (create-level self))
         (message-logger (get-message-logger self))
         (available-positions '()))
    (dotimes (x (get-width level))
      (dotimes (y (get-height level))
        (when (is-accessible-p (get-cell-at level x y))
          (add-to-list 'available-positions (cons x y)))))
    (set-level hero level)

    (let ((hero-position (nth (random (length available-positions))
                              available-positions)))
      (set-pos hero (car hero-position) (cdr hero-position)))
    (set-message-logger hero message-logger)

    (let ((difficulty-number nil)
          (enemies nil))
      (while (not (numberp difficulty-number))
        (setq difficulty-number (read-minibuffer "Enter a difficulty (number): ")))

      (setq enemies (rlk--level-populator-arena-populate-level level
                                                               (+ 30
                                                                  (* difficulty-number
                                                                     12))
                                                               5))
      (oset self enemy-count (length enemies))

      (dolist (enemy enemies)
        (set-message-logger enemy message-logger)
        (set-target (get-behaviour enemy) hero)
        (register (get-dispatcher enemy)
                  :died
                  (apply-partially 'decrement-enemy-count self))))))

(cl-defmethod decrement-enemy-count ((self rlk--game-screen-arena))
  "Decrement by one the enemy count.

If the count reaches 0, the player wins the game."
  (oset self enemy-count (1- (oref self enemy-count)))
  (when (<= (oref self enemy-count) 0)
    (win self)))

(provide 'roguel-ike/game-screen/arena)

;;; arena.el ends here
