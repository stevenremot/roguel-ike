;;; periodic.el --- Periodically add entities to a level

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
(require 'roguel-ike/level/populator/arena)

(defclass rlk--level-populator-periodic ()
  ((level :initarg :level
          :type rlk--level
          :reader get-level
          :protection :private
          :documentation "The level on which the populator spawns entities.")
   (spawn-period :initarg :spawn-period
                 :initform 30
                 :reader get-spawn-period
                 :protection :private
                 :documentation "Return the number of turns before the populator create a new entity.")
   (counter :initform 0
            :protection :private
            :documentation "The current turn count.")
   (difficulty :initarg :difficulty
               :type integer
               :reader get-difficulty
               :protection :private
               :documentation "The level of the spawned entities.")
   (hero :initarg :hero
         :type rlk--entity
         :reader get-hero
         :protection :private
         :documentation "Return the hero the populator uses to count turns.")
   (message-logger :initarg :message-logger
                   :type rlk--message-logger
                   :reader get-message-logger
                   :protection :private
                   :documentation "The message logger to set to entities."))
  "Periodically add entities to a level.")

(defmethod initialize-instance :after ((self rlk--level-populator-periodic) slots)
  (register (get-dispatcher (get-hero self)) :turns-spent
            (apply-partially 'register-turn self)))

(defmethod spawn-entity ((self rlk--level-populator-periodic))
  "Spawn a new entity in the level."
  (let ((entity (car (rlk--level-populator-arena-populate-level (get-level self)
                                                                (* 30 (1+ (get-difficulty self)))
                                                                1))))
    (set-message-logger entity (get-message-logger self))
    (set-target (get-behaviour entity) (get-hero self))))

(defmethod register-turn ((self rlk--level-populator-periodic) nb-turns)
  "Register a turn, and spawn entities if there have been enough time spent."
  (when (eq (get-level self) (get-level (get-hero self)))
    (oset self counter (+ nb-turns (oref self counter)))
    (while (> (oref self counter) (get-spawn-period self))
      (spawn-entity self)
      (oset self counter (- (oref self counter) (get-spawn-period self))))))

(provide 'roguel-ike/level/populator/periodic)

;;; periodic.el ends here
