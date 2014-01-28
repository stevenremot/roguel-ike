;;; roguel-ike-physics.el --- Simple physics simulation

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
;; The game implements a simple physics simulator.
;; Some objects can have a motion that can make them move.

;;; Code:

(require 'eieio)
(require 'roguel-ike-math)

;;;;;;;;;;;;;;
;; Generics ;;
;;;;;;;;;;;;;;

;; An object must implement these methods to be affected by a motion

(defgeneric get-x (object)
  "Return the horizontal position of an object.")

(defgeneric get-y (object)
  "Return the vertical position of an object.")

(defgeneric try-move (object dx dy)
  "Try to move the object to the position x+DX, y+DY.
Return t if it succeeded, nil otherwise.")

(defgeneric get-neighbour-cell (object dx dy)
  "Return the cell at position x+DX, y+DY.")

(defgeneric collide-with-cell (object cell direction energy)
  "Apply action on collision with CELL.
DIRECTION is the base motion direction, and ENERGY is the current motion energy.")

;;;;;;;;;;;;
;; Motion ;;
;;;;;;;;;;;;

(defclass rlk--physics-motion ()
  ((object :initarg :object
           :reader get-object
           :protection :private
           :documentation "The object on which the motion applies.")
   (energy :initarg :energy
           :reader get-energy
           :protection :private
           :documentation "The duration of the movement.")
   (direction :initarg :direction
              :type rlk--math-point
              :reader get-direction
              :protection :private
              :documentation "The direction of the movement."))
  "A motion is a attenuating movement applied to a given object.")

(defmethod update ((self rlk--physics-motion))
  "Move the entity for one turn.
Apply all effects, including collisions and movement attenuation."
  (let* ((object (get-object self))
         (direction (get-direction self))
         (base-pos (rlk--math-point "Base position"
                                    :x (get-x object)
                                    :y (get-y object)))
         (next-cell (get-neighbour-cell object
                                        (get-x direction)
                                        (get-y direction))))
    (if (try-move object (get-x direction) (get-y direction))
        (oset self energy (1- (get-energy self)))
      (progn
        (collide-with-cell object next-cell direction (get-energy self))
        (oset self energy 0)))))

;;;;;;;;;;;
;; World ;;
;;;;;;;;;;;

(defclass rlk--physics-world ()
  ((motions :initform ()
            :type list
            :reader get-motions
            :protection :private
            :documentation "World's motions."))
  "Aggregate and update motions.")

(defmethod add-motion ((self rlk--physics-world) motion)
  "Add a MOTION to the world."
  (let ((motions (get-motions self)))
    (oset self motions (add-to-list 'motions motion))))

(defmethod remove-motion ((self rlk--physics-world) motion)
  "Remove a MOTION from the world."
  (let ((new-motions '()))
    (dolist (old-motion (get-motions self))
      (unless (equal motion old-motion)
        (add-to-list 'new-motions old-motion)))
    (oset self motions new-motions)))

(defmethod do-step ((self rlk--physics-world))
  "Update MOTIONS for one turn.
Return t if at least one MOTION remains, nil otherwise."
  (let ((motions (get-motions self)))
    (dolist (motion motions)
      (update motion)
      (when (= (get-energy motion) 0)
        (remove-motion self motion)))
    (> (length (get-motions self)) 0)))

(defmethod run ((self rlk--physics-world) draw-callback)
  "Update bodies as long as at least one is moving.
DRAW-CALLBACK is called at each iteration."
  (when (do-step self)
    (funcall draw-callback)
    (sit-for 0.1)
    (run self draw-callback)))

(provide 'roguel-ike-physics)
;;; roguel-ike-physics.el ends here
