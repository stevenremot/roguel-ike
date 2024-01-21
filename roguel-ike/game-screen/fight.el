;;; fight.el --- Base class for in-game screens

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
(require 'roguel-ike/mode)
(require 'roguel-ike/game-screen)
(require 'roguel-ike/controller)
(require 'roguel-ike/message-logger)
(require 'roguel-ike/entity/factory/hero)
(require 'roguel-ike/game)
(require 'roguel-ike/graphics/renderer/game)
(require 'roguel-ike/graphics/renderer/stats)
(require 'roguel-ike/controller)

(defvar-local rlk--local-controller nil
  "Game controller associated to the buffer.")

(defclass rlk--game-screen-fight (rlk--game-screen)
  ((controller :type rlk--controller
               :reader get-controller
               :protection :private
               :documentation "Game controller.")
   (message-logger :type rlk--message-logger
                   :reader get-message-logger
                   :protection :private
                   :documentation "Message elogging system.")
   (base-hero-data :type rlk--hero-data
                   :reader get-base-hero-data
                   :protection :protected
                   :documentation "Hero data at the beginning of the fight."))
  "Base game screen for all fighting screens.")

(cl-defmethod setup ((self rlk--game-screen-fight) hero-data)
  (let* ((buffer-manager (get-buffer-manager self))
         (message-logger (rlk--message-logger :message-buffer (get-message-buffer buffer-manager)))
         (hero (rlk--entity-create-from-hero-data hero-data))
         (game (rlk--game :hero hero
                          :buffer-manager buffer-manager))
         (stats-renderer (rlk--graphics-renderer-stats :buffer (get-stats-buffer buffer-manager)
                                                       :entity hero))
         (game-renderer (rlk--graphics-renderer-game :buffer (get-game-buffer buffer-manager)))
         (controller (rlk--controller :game game
                                      :game-renderer game-renderer
                                      :stats-renderer stats-renderer)))
    (oset self controller controller)
    (oset self message-logger message-logger)
    (oset self base-hero-data hero-data)

    (setup-level self)
    (register (get-dispatcher hero) :died (apply-partially 'loose self))

    (clear message-logger)
    (setup-game-layout buffer-manager)
    (set-buffer (get-game-buffer buffer-manager))
    (rlk--fight-mode)
    (setq rlk--local-controller controller)
    (do-step (get-time-manager (get-current-level game)))))

(cl-defmethod setup-level ((self rlk--game-screen-fight))
  "Abstract method for creating level and setting all its elements."
  (error "The method setup-level must be overriden"))

(cl-defmethod win ((self rlk--game-screen-fight))
  "Called when the game is won."
  (display-message (get-message-logger self) "You win!")
  (end-fight self (rlk--entity-create-hero-data
                      (get-name (get-base-hero-data self))
                      (get-hero (get-game (get-controller self))))))

(cl-defmethod loose ((self rlk--game-screen-fight))
  "Called when the game is lost."
  (display-message (get-message-logger self) "You lost.")
  (end-fight self (get-base-hero-data self)))

(cl-defmethod end-fight ((self rlk--game-screen-fight) hero-data)
  "Stop the level and return to the mode selection screen.

Return HERO-DATA to mode selection screen."
  (stop (get-current-level (get-game (get-controller self))))
  (read-key-sequence "Press any key to leave")
  (call-end-callback self
                     'rlk--game-screen-select-mode
                     hero-data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; associated major mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rlk--fight-mode-map (get-keymap rlk--controller)
  "Keymap for roguel-ike fighting mode.")

(define-derived-mode rlk--fight-mode rlk--mode "roguel-ike:fight"
  "Roguel-ike fighting major mode.")

(provide 'roguel-ike/game-screen/fight)

;;; fight.el ends here
