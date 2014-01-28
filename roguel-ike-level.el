;;; level.el --- Contains the data structures relative to levels

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
;; Defines roguel-ike level structure

;;; Code:
(require 'eieio)
(require 'roguel-ike-time)
(require 'roguel-ike-physics)

(defvar-local rlk-controller nil)

;;;;;;;;;;;;
;; Object ;;
;;;;;;;;;;;;

(defclass rlk--level-cell-object ()
  ((type :initarg :type
         :reader get-type
         :type symbol
         :protection :protected
         :documentation "The intrisic type of the object.")
   (x :initform -1
      :type integer
      :reader get-x
      :protection :private
      :documentation "The horizontal position of the object in the level.")
   (y :initform -1
      :type integer
      :reader get-y
      :protection :private
      :documentation "The vertical position of the object in the level.")
   (level :reader get-level
          :writer set-level
          :type rlk--level
          :protection :protected
          :documentation "The level which contains the object."))
  "Represent an object lying on a cell."
  :abstract t)

(defmethod is-entity-p ((self rlk--level-cell-object))
  "Return t if the object is an entity, nil otherwise."
  nil)

(defmethod get-layer ((self rlk--level-cell-object))
  "Return the layer on which the object is drawn."
  (error "Method get-layer must be overriden"))

(defmethod accept-other-object-p ((self rlk--level-cell-object))
  "Return t if another object can stand on the cell, nil otherwise."
  (error "Method accept-other-object-p must be overriden"))

(defmethod block-light-p ((self rlk--level-cell-object))
  "Return t if the OBJECT blocks the light, nil otherwise."
  nil)

(defmethod get-cell ((self rlk--level-cell-object))
  "Return the cell on which stands the object."
  (get-cell-at (get-level self)
               (get-x self)
               (get-y self)))

(defmethod get-neighbour-cell ((self rlk--level-cell-object) dx dy)
  "Return the cell at the position x+DX, y+DY."
  (get-cell-at (get-level self)
               (+ (get-x self) dx)
               (+ (get-y self) dy)))

(defmethod set-cell ((self rlk--level-cell-object) cell)
  "Set the object's cell.
This method is intended for private use ONLY.
Use set-pos to change the object's position."
  (let ((old-cell (get-cell self)))
    (when (is-container-p old-cell)
      (remove-object old-cell self))
    (add-object cell self)))

(defmethod set-pos ((self rlk--level-cell-object) x y)
  "Set the new cell pos."
  (let ((cell (get-cell-at (get-level self) x y)))
    (when cell
      (set-cell self cell)
      (oset self x x)
      (oset self y y))))

;;;;;;;;;;
;; Cell ;;
;;;;;;;;;;

(defclass rlk--level-cell ()
  ((type :initarg :type
         :reader get-type
         :writer set-type
         :type symbol
         :protection :protected
         :documentation "The intrinsic type of the cell
e.g. wall, ground, etc...")
   (lit :initform nil
        :type boolean
        :reader is-lit-p
        :writer set-lit
        :protection private
        :documentation "Tells wether the cell is lit or not.")
   (visited :initform nil
            :type boolean
            :reader is-visited-p
            :writer set-visited
            :protection :private
            :documentation "Tells if the cell has been seen or not."))
  "A class representing a level's cell")

(defmethod is-container-p ((self rlk--level-cell))
  "Return t if the cell can contain objects, nil otherwise."
  nil)

(defmethod is-accessible-p ((self rlk--level-cell))
  "Return t if the cell can be the destination of an entity, nil otherwise."
  nil)

(defmethod block-light-p ((self rlk--level-cell))
  "Return t if the cell blocks the light, nil otherwise."
  t)

;;;;;;;;;;;;;;;;;
;; Ground cell ;;
;;;;;;;;;;;;;;;;;

(defclass rlk--level-cell-ground (rlk--level-cell)
  ((type :initform :ground
         :protection :protected)
   (objects :initform ()
            :reader get-objects
            :writer set-objects
            :type list
            :protection :private
            :documentation "All the objects lying on the cell."))
  "A ground cell")

(defmethod is-container-p ((self rlk--level-cell-ground))
  "Return t if the cell can contain objects, nil otherwise."
  t)

(defmethod add-object ((self rlk--level-cell-ground) object)
  "Add OBJECT into CELL's objects if it is not already in."
  (let ((objects (get-objects self)))
    (set-objects self (add-to-list 'objects object))))

(defmethod remove-object ((self rlk--level-cell-ground) object)
  "Remove OBJECT from CELL's objects if it is in."
  (set-objects self (delete object (get-objects self))))

(defmethod get-entity ((self rlk--level-cell-ground))
  "Return the entity on the CELL if there is currently one.
Return nil otherwise."
  (catch 'entity
    (dolist (object (get-objects self))
      (when (is-entity-p object)
        (throw 'entity object)))
    (throw 'entity nil)))

(defmethod set-entity ((self rlk--level-cell-ground) entity)
  "Set the ENTITY standing on the CELL.
If there is already an entity on it, it will be removed."
  (let ((old-entity (get-entity self)))
    (when old-entity
      (remove-object self old-entity))
    (when entity
      (add-object self entity))))

(defmethod has-entity-p ((self rlk--level-cell-ground))
  "Return `t' if the cell contains an entity, nil otherwise"
  (rlk--level-cell-object-child-p (get-entity self)))

(defmethod get-highest-layer-object ((self rlk--level-cell-ground))
  "Return the object on the highest layer.
If there is no object, return nil."
  (let ((highest-object nil)
        (layer nil))
    (dolist (object (get-objects self))
      (when (or (not highest-object)
                (> (get-layer object) layer))
        (setq highest-object object)
        (setq layer (get-layer object))))
  highest-object))

(defmethod is-accessible-p ((self rlk--level-cell-ground))
  "Return t if cell can accept a new object, nil otherwise."
  (catch 'accessible
    (dolist (object (get-objects self))
      (unless (accept-other-object-p object)
        (throw 'accessible nil)))
    t))

(defmethod block-light-p ((self rlk--level-cell-ground))
  "See rlk--level-cell."
  (catch 'block-light
    (dolist (object (get-objects self))
      (when (block-light-p object)
        (throw 'block-light t)))
    nil))

;;;;;;;;;;;;
;; Level  ;;
;;;;;;;;;;;;

(defclass rlk--level ()
  ((cells :initarg :cells
          :type list
          :reader get-cells
          :protection :private
          :documentation "A two-dimensional list of cells")
   (time-manager :type rlk--time-manager
                 :reader get-time-manager
                 :protection :private
                 :documentation "Time management algorithm.")
   (physics-world :type rlk--physics-world
                  :reader get-physics-world
                  :protection :private
                  :documentation "Physics world."))
  "Represents a game level")

(defmethod initialize-instance :after ((self rlk--level) slots)
  "Initialize the time manager."
  (oset self time-manager (rlk--time-manager "Level time manager"))
  (oset self physics-world (rlk--physics-world "Physics world"))
  (add-after-step-hook (get-time-manager self)
                       (apply-partially (lambda (self)
                                          (run-world self))
                                        self)))

(defmethod width ((self rlk--level))
  "Return the horizontal number of cells."
  (length (oref self cells)))

(defmethod height ((self rlk--level))
  "Return the vertical number of cells."
  (if (eq (width self) 0)
      0
    (length (car (oref self cells)))))

(defmethod get-cell-at ((self rlk--level) x y)
  "Return the cell at position x, y."
  (nth x (nth y (get-cells self))))

(defmethod get-controller ((self rlk--level))
  "Return the current controller."
  rlk-controller)

(defmethod add-entity ((self rlk--level) entity)
  "Add an entity to the level."
  (insert-object (get-time-manager self) entity))

(defmethod remove-entity ((self rlk--level) entity)
  "Remove an entity from the level."
  (remove-object (get-time-manager self) entity))

(defmethod add-motion ((self rlk--level) entity direction energy)
  "Create a motion affecting ENTITY, for the given DIRECTION and ENERGY.

if DIRECTION is a cons in the form (DX, DY), it will be converted to
a rlk--math-point. This avoids useless dependencies with rlk--math."
  (when (consp direction)
    (setq direction (rlk--math-point "Motion direction"
                                     :x (car direction)
                                     :y (cdr direction))))
  (add-motion (get-physics-world self) (rlk--physics-motion "Motion"
                                                            :object entity
                                                            :direction direction
                                                            :energy energy)))

(defmethod run-world ((self rlk--level))
  "Update the world's motions."
  (run (get-physics-world self) (apply-partially 'call-renderers
                                                 (get-controller self))))

(provide 'roguel-ike-level)
;;; roguel-ike-level.el ends here
