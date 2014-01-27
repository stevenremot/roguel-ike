;;; roguel-ike.el --- Main file for roguel-ike

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
;; Contains launch command

;;; Code:

(require 'roguel-ike-game)
(require 'roguel-ike-buffer)
(require 'roguel-ike-message)
(require 'roguel-ike-graphics)
(require 'roguel-ike-controller)
(require 'roguel-ike-interactive-object)
(require 'roguel-ike-entity)
(require 'roguel-ike-behaviour)
(require 'roguel-ike-race)

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

;;;###autoload
(defun roguel-ike ()
  "Start a roguel-ike game."
  (interactive)
  (let* ((buffer-manager (rlk--buffer-manager "Buffer manager"))
         (layout '("############"
                   "#..#####...#"
                   "#....#...###"
                   "###......###"
                   "############"))
         (cells (rlk--get-cells-from-layout layout))
         (level (rlk--level "Level" :cells cells))
         (message-logger (rlk--message-logger "Message logger"
                                              :message-buffer (get-message-buffer buffer-manager)))
         (hero (rlk--entity-create-new (rlk--race-get-race :human)
                                       (rlk--behaviour-manual "Manual behaviour")
                                       message-logger))
         (rat (rlk--entity-create-new (rlk--race-get-race :rat)
                                      (rlk--behaviour-ai "AI behaviour")
                                      message-logger)) ;; TODO replace this by a monster dropper or random level generation
         (door (rlk--interactive-object-door "Door")) ;; TODO remove this after random level generation
         (game (rlk--game "Game"
                          :level level
                          :hero hero
                          :buffer-manager buffer-manager))
         (stats-renderer (rlk--graphics-renderer-stats "Stats renderer"
                                                       :buffer (get-stats-buffer buffer-manager)
                                                       :stats(get-stats hero)))
         (game-renderer (rlk--graphics-renderer-game "Game renderer"
                                                     :buffer (get-game-buffer buffer-manager)))
         (game-controller (rlk--controller-game "Game controller"
                                                :game game
                                                :game-renderer game-renderer
                                                :stats-renderer stats-renderer)))
    (set-level hero level)
    (set-pos hero 1 1)
    (set-level rat level)
    (set-pos rat 9 1)

    (add-entity level hero)
    (add-entity level rat)

    (add-object (get-cell-at level 5 3) door)

    (setup-layout buffer-manager)
    (draw-stats stats-renderer)
    (display-message message-logger "Welcome, young adventurer!")
    (setup game-controller)
    (call-renderers game-controller)
    (do-step (get-time-manager level))))

(provide 'roguel-ike)

;;; roguel-ike.el ends here
