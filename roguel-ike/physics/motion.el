;;; motion.el --- Physical motion

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
;; A physical motion make an object move in the level.

;;; Code:
(require 'cl-generic)
(require 'eieio)

;;;;;;;;;;;;;;
;; Generics ;;
;;;;;;;;;;;;;;

;; An object must implement these methods to be affected by a motion

(cl-defgeneric get-x (object)
  "Return the horizontal position of an object.")

(cl-defgeneric get-y (object)
  "Return the vertical position of an object.")

(cl-defgeneric try-move (object dx dy)
  "Try to move the object to the position x+DX, y+DY.
Return t if it succeeded, nil otherwise.")

(cl-defgeneric get-neighbour-cell (object dx dy)
  "Return the cell at position x+DX, y+DY.")

(cl-defgeneric collide-with-cell (object cell direction energy)
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
           :type (or integer null)
           :reader get-energy
           :protection :private
           :documentation "The duration of the movement.

A null value means the movement won't stop until it bumps into something.")
   (direction :initarg :direction
              :type cons
              :reader get-direction
              :protection :private
              :documentation "The direction of the movement."))
  "A motion is a attenuating movement applied to a given object.")

(cl-defmethod update ((self rlk--physics-motion))
  "Move the entity for one turn.
Apply all effects, including collisions and movement attenuation."
  (let* ((object (get-object self))
         (direction (get-direction self))
         (base-pos (cons (get-x object)
                         (get-y object)))
         (next-cell (get-neighbour-cell object
                                        (car direction)
                                        (cdr direction))))
    (if (try-move object (car direction) (cdr direction))
        (when (get-energy self)
          (oset self energy (1- (get-energy self))))
      (progn
        (collide-with-cell object next-cell direction (get-energy self))
        (oset self energy 0)))))

(provide 'roguel-ike/physics/motion)
;;; motion.el ends here
