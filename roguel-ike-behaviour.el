;;; roguel-ike-behaviour.el --- Entities' behaviour

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

;;; Code:

(require 'eieio)
(require 'roguel-ike-entity)

;;;;;;;;;;;;;;;;
;; Base class ;;
;;;;;;;;;;;;;;;;

(defclass rlk--behaviour ()
  ((entity :reader get-entity
           :writer set-entity
           :protection :private
           :documentation "THe tntiy the current behaviour controls."))
  "Base class for behaviour objects."
  :abstract t)

(defmethod do-action ((self rlk--behaviour) callback)
  "Decide which action should be done now.
Must call callback with the number of turns the action takes."
  (error "Method do-action for behaviour must be overriden"))

;;;;;;;;;;;;;;;;;;
;; Manual class ;;
;;;;;;;;;;;;;;;;;;

(defgeneric call-renderers (controller)
  "Call the game's renderers.")

(defvar-local rlk-controller nil)

(defclass rlk--behaviour-manual (rlk--behaviour)
  ((time-callback :type function
                  :reader get-time-callback
                  :protection :private
                  :documentation "The callback sent by the time manager."))
  "Behaviour of entities controlled by the player.")

(defmethod get-controller ((self rlk--behaviour-manual))
  "Return the behaviour's controller.

It would be more elegant to avoid using the global variable, but it leads to
cyclic dependencies.

behaviour <-- hero <--- game <-- controller
   |---------------------------------A"
  rlk-controller)

(defmethod interact-with-cell ((self rlk--behaviour-manual) dx dy)
  "Try all sort of interaction with cell at DX, DY.

If cell is accessible, will move to it.
If not, and it has a door, will open it.

Apply the time callback."
  (let ((entity (get-entity self)))
    (funcall (oref self time-callback)
             (let* ((x (+ (get-x entity) dx))
                    (y (+ (get-y entity) dy))
                    (cell (get-cell-at (get-level entity) x y)))
               (if (is-accessible-p cell)
                   (if (try-move entity dx dy)
                       1
                     0)
                 (if (has-entity-p cell)
                     (progn
                       (attack (get-entity self) (get-entity cell))
                       1)
                   (if (is-container-p cell)
                       (catch 'time
                         (dolist (object (get-objects cell))
                           (when (equal (get-type object) :door-closed)
                             (do-action object entity :open)
                             (display-message entity "You open the door.")
                             (throw 'time 1)))
                         0)
                     0)))
               0))))

(defmethod wait ((self rlk--behaviour-manual))
  "Wait one turn."
  (funcall (oref self time-callback) 1))

(defmethod do-action ((self rlk--behaviour-manual) callback)
  "Register the callback for a former use."
  (call-renderers (get-controller self))
  (oset self time-callback callback))

;;;;;;;;;;;;;;
;; AI class ;;
;;;;;;;;;;;;;;

(defclass rlk--behaviour-ai (rlk--behaviour)
  ()
  "Behaviour of entities controlled by the computer.")


(defmethod move-randomly ((self rlk--behaviour-ai))
  "Try to move on a random neighbour cell.
Return the number of turns spent if it could move, 1 for waiting otherwise."
  (let* ((entity (get-entity self))
         (accessible-cells '())
         (level (get-level entity))
         (choosen-cell nil))
    (dotimes (i 3)
      (dotimes (j 3)
        (let*
            ((dx (- i 1))
             (dy (- j 1))
             (x (+ (get-x entity) (- i 1)))
             (y (+ (get-y entity) (- j 1)))
             (cell (get-cell-at level x y)))
          (when (is-accessible-p cell)
            (add-to-list 'accessible-cells (cons dx dy))))))
    ;; If there are accessible cells, move. Otherwise, wait.
    (when accessible-cells
      (setq choosen-cell (nth (random (length accessible-cells))
                              accessible-cells))
      (try-move entity (car choosen-cell) (cdr choosen-cell)))
      1))

(defmethod do-action ((self rlk--behaviour-ai) callback)
  "See rlk--behaviour."
  (funcall callback (move-randomly self)))



(provide 'roguel-ike-behaviour)

;;; roguel-ike-behaviour.el ends here
