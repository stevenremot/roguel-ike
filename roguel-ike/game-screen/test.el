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
(require 'roguel-ike/game-screen)
(require 'roguel-ike/hero-data/manager)
(require 'roguel-ike/entity/factory/hero)
(require 'roguel-ike/game)
(require 'roguel-ike/buffer-manager)
(require 'roguel-ike/message-logger)
(require 'roguel-ike/graphics/renderer/game)
(require 'roguel-ike/graphics/renderer/stats)
(require 'roguel-ike/controller)
(require 'roguel-ike/interactive-object/door)
(require 'roguel-ike/entity)
(require 'roguel-ike/behaviour/ai)


(defclass rlk--game-screen-test (rlk--game-screen)
  ()
  "A test screen that creates a little level.")

(defmethod setup ((self rlk--game-screen-test) hero-data-manager hero-data)
  (let* ((buffer-manager (get-buffer-manager self))
         (layout '("############"
                   "#..#####...#"
                   "#....#...###"
                   "###......###"
                   "############"))
         (cells (rlk--get-cells-from-layout layout))
         (level (rlk--level "Level" :cells cells))
         (message-logger (rlk--message-logger "Message logger"
                                              :message-buffer (get-message-buffer buffer-manager)))
         (hero (rlk--entity-create-from-hero-data hero-data))
         (rat (rlk--entity-create-new :rat
                                      (rlk--behaviour-ai "AI behaviour"))) ;; TODO replace this by a monster dropper or random level generation
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
         (game-controller (rlk--controller "Game controller"
                                           :game game
                                           :game-renderer game-renderer
                                           :stats-renderer stats-renderer)))
    (set-level hero level)
    (set-pos hero 1 1)
    (set-level rat level)
    (set-pos rat 9 1)

    (add-entity level hero)
    (add-entity level rat)

    (set-message-logger hero message-logger)
    (set-message-logger rat message-logger)

    (add-object (get-cell-at level 5 3) door)

    (setup-game-layout buffer-manager)
    (draw-stats stats-renderer)
    (display-message message-logger "Welcome, young adventurer!")
    (setup game-controller)
    (call-renderers game-controller)
    (do-step (get-time-manager level))))

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


(provide 'roguel-ike/game-screen/test)

;;; test.el ends here
