;;; manual.el --- Manual entity control

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
;; The in-world representation of an entity and its behaviour
;; are clearly separated.  The behaviour of an entity is the
;; object that decides what an entity should do now, regarding
;; its current environment.

;;; Commentary:
;; This behaviour let the player control an entity.

;;; Code:
(require 'cl-generic)
(require 'roguel-ike/behaviour)
(require 'roguel-ike/interactive-object/door)
(require 'roguel-ike/level/cell/ground)

(cl-defgeneric call-renderers (controller)
  "Call the game's renderers.")

(cl-defgeneric ask-option (controller prompt collection)
  "Ask the user to select an option in COLLECTION.")

(cl-defgeneric ask-direction (controller)
  "Ask the user for a direction.")

(defvar-local rlk--local-controller nil)

(defclass rlk--behaviour-manual (rlk--behaviour)
  ((time-callback :type function
                  :reader get-time-callback
                  :protection :private
                  :documentation "The callback sent by the time manager."))
  "Behaviour of entities controlled by the player.")

(cl-defmethod is-manual-p ((self rlk--behaviour-manual))
  "See rlk--behaviour."
  t)

(cl-defmethod get-controller ((self rlk--behaviour-manual))
  "Return the behaviour's controller.

It would be more elegant to avoid using the global variable, but it leads to
cyclic dependencies.

behaviour <-- hero <--- game <-- controller
   |---------------------------------A"
  rlk--local-controller)

(cl-defmethod spend-time ((self rlk--behaviour-manual) time)
  "Call the time callback function with given TIME."
  (spend-time (get-entity self) time)
  (funcall (oref self time-callback) time))

(cl-defmethod interact-with-cell ((self rlk--behaviour-manual) dx dy)
  "Try all sort of interaction with cell at DX, DY.

If cell is accessible, will move to it.
If not, and it has a door, will open it.

Apply the time callback."
  (let ((entity (get-entity self)))
    (spend-time self
                (let* ((cell (get-neighbour-cell entity dx dy)))
                  (if (is-accessible-p cell)
                      (if (try-move entity dx dy)
                          1
                        0)
                    (if (is-container-p cell)
                        (if (has-entity-p cell)
                            (progn
                              (attack (get-entity self) (get-entity cell))
                              1)
                          (catch 'time
                            (dolist (object (get-objects cell))
                              (when (equal (get-type object) :door-closed)
                                (do-action object entity :open)
                                (display-message entity "You open the door.")
                                (throw 'time 1)))
                            0))
                      0))))))

(cl-defmethod wait ((self rlk--behaviour-manual))
  "Wait one turn."
  (spend-time self 1))

(cl-defmethod do-action ((self rlk--behaviour-manual) callback)
  "Register the callback for a former use."
  (let ((controller (get-controller self)))
    (when controller
      (call-renderers controller))
    (oset self time-callback callback)
    nil))

(cl-defmethod close-door ((self rlk--behaviour-manual) dx dy)
  "Try to close the door in the direction DX, DY."
  (let* ((entity (get-entity self))
         (cell (get-neighbour-cell entity dx dy)))
    (if (is-container-p cell)
        (let ((door (catch 'door
                      (dolist (object (get-objects cell))
                        (when (rlk--interactive-object-door-p object)
                          (throw 'door object)))
                      nil)))
          (if door
              (if (is-opened-p door)
                  (if (not (has-entity-p cell))
                      (progn
                        (do-action door entity :close)
                        (display-message entity "You close the door.")
                        (spend-time self 1))
                    (display-message entity "There is something on the way."))
                (display-message entity "The door is already closed."))
            (display-message entity "There is no door here...")))
      (display-message entity "There is no door here..."))))

(cl-defmethod climb-stairs ((self rlk--behaviour-manual))
  "If there are stairs on the entity's cell, climb them."
  (let* ((entity (get-entity self))
         (cell (get-cell entity))
         (stairs (catch 'stairs
                   (dolist (object (get-objects cell))
                     (when (member (get-type object) '(:stairs-up :stairs-down))
                       (throw 'stairs object)))
                   nil)))
    (if stairs
        (do-action stairs entity :climb)
      (display-message entity "There are no stairs here..."))))

(cl-defmethod select-and-use-skill ((self rlk--behaviour-manual))
  "Ask the user to select a skill and use it."
  (let* ((entity (get-entity self))
         (skills (get-usable-skills entity))
         (choosen-skill-name ""))
    (if skills
        (progn
          (setq choosen-skill-name (ask-option (get-controller self)
                                               "Which skill to use: "
                                               (mapcar (lambda (skill)
                                                         (get-name skill))
                                                       skills)))
          (when choosen-skill-name
            (catch 'skill-found
              (dolist (skill skills)
                (when (string-equal choosen-skill-name (get-name skill))
                  (use-skill self skill)
                  (throw 'skill-found nil))))))
      (display-message entity "You cannot use any skill now."))))

(cl-defmethod use-skill ((self rlk--behaviour-manual) skill)
  "Try to use the skill SKILL.

If it could be used, spend a turn for it."
  (let ((arguments '())
        (direction nil))
    (when (has-tag-p skill :directional)
      (setq direction (ask-direction (get-controller self)))
      (setq arguments (append arguments (list (car direction) (cdr direction)))))
    (when (apply 'use-skill (get-entity self) skill arguments)
      (spend-time self 1))))

(provide 'roguel-ike/behaviour/manual)

;;; manual.el ends here
