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

;;;;;;;;;;;;
;; Object ;;
;;;;;;;;;;;;

(defclass rlk--level-cell-object ()
  ((type :reader get-type
         :type symbol
         :protection :protected
         :documentation "The intrisic type of the object."))
  "Represent an object lying on a cell."
  :abstract t)

(defmethod is-entity-p ((object rlk--level-cell-object))
  "Return t if the object is an entity, nil otherwise."
  nil)

(defmethod get-layer ((object rlk--level-cell-object))
  "Return the layer on which the object is drawn."
  (error "Method get-layer must be overriden"))

(defmethod accept-other-object-p ((object rlk--level-cell-object))
  "Return t if another object can stand on the cell, nil otherwise."
  (error "Method accept-other-object-p must be overriden"))

(defmethod block-light-p ((object rlk--level-cell-object))
  "Return t if the OBJECT blocks the light, nil otherwise."
  nil)

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

(defmethod is-container-p ((cell rlk--level-cell))
  "Return t if the cell can contain objects, nil otherwise."
  nil)

(defmethod is-accessible-p ((cell rlk--level-cell))
  "Return t if the cell can be the destination of an entity, nil otherwise."
  nil)

(defmethod block-light-p ((cell rlk--level-cell))
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

(defmethod is-container-p ((cell rlk--level-cell-ground))
  "Return t if the cell can contain objects, nil otherwise."
  t)

(defmethod add-object ((cell rlk--level-cell-ground) object)
  "Add OBJECT into CELL's objects if it is not already in."
  (let ((objects (get-objects cell)))
    (set-objects cell (add-to-list 'objects object))))

(defmethod remove-object ((cell rlk--level-cell-ground) object)
  "Remove OBJECT from CELL's objects if it is in."
  (set-objects cell (delete object (get-objects cell))))

(defmethod get-entity ((cell rlk--level-cell-ground))
  "Return the entity on the CELL if there is currently one.
Return nil otherwise."
  (catch 'entity
    (dolist (object (get-objects cell))
      (when (is-entity-p object)
        (throw 'entity object)))
    (throw 'entity nil)))

(defmethod set-entity ((cell rlk--level-cell-ground) entity)
  "Set the ENTITY standing on the CELL.
If there is already an entity on it, it will be removed."
  (let ((old-entity (get-entity cell)))
    (when old-entity
      (remove-object cell old-entity))
    (when entity
      (add-object cell entity))))

(defmethod has-entity-p ((cell rlk--level-cell-ground))
  "Return `t' if the cell contains an entity, nil otherwise"
  (rlk--level-cell-object-child-p (get-entity cell)))

(defmethod get-highest-layer-object ((cell rlk--level-cell-ground))
  "Return the object on the highest layer.
If there is no object, return nil."
  (let ((highest-object nil)
        (layer nil))
    (dolist (object (get-objects cell))
      (when (or (not highest-object)
                (> (get-layer object) layer))
        (setq highest-object object)
        (setq layer (get-layer object))))
  highest-object))

(defmethod is-accessible-p ((cell rlk--level-cell-ground))
  "Return t if cell can accept a new object, nil otherwise."
  (catch 'accessible
    (dolist (object (get-objects cell))
      (unless (accept-other-object-p object)
        (throw 'accessible nil)))
    (throw 'accessible t)))

(defmethod block-light-p ((cell rlk--level-cell-ground))
  "See rlk--level-cell."
  (catch 'block-light
    (dolist (object (get-objects cell))
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
   (enemies :initform ()
            :type list
            :reader get-enemies
            :writer set-enemies
            :protection :private
            :documentation "Living entities hostiles to the player."))
  "Represents a game level")

(defmethod width ((level rlk--level))
  "Return the horizontal number of cells."
  (length (oref level cells)))

(defmethod height ((level rlk--level))
  "Return the vertical number of cells."
  (if (eq (width level) 0)
      0
    (length (car (oref level cells)))))

(defmethod get-cell-at ((level rlk--level) x y)
  "Return the cell at position x, y."
  (nth x (nth y (get-cells level))))

(defmethod add-enemy ((level rlk--level) enemy)
  "Add an enemy to the enemies list."
  (let ((enemies (get-enemies level)))
    (set-enemies level (add-to-list 'enemies enemy))))

(defmethod remove-enemy ((level rlk--level) enemy)
  "Remove an enemy from the enemies list."
  (set-enemies level (delete enemy (get-enemies level))))

(defmethod update-enemies ((level rlk--level))
  "Let enemies spend their time delay."
  (let (enemies-to-update '())
    ;; Get all enemies with a negative time delay
    (dolist (enemy (get-enemies level))
      (when (can-do-action enemy)
        (add-to-list 'enemies-to-update enemy)))
    ;; Update enemies one by one, turn by turn, as long as there is one
    ;; with time to spend
    (while enemies-to-update
      (let ((enemies enemies-to-update))
        (dolist (enemy enemies)
          (update enemy)
          (unless (can-do-action enemy)
            (setq enemies-to-update (delete enemy enemies-to-update))))))))

(defmethod add-time-delay-enemies ((level rlk--level) time)
  "Add TIME to all enemies' time delay."
  (dolist (enemy (get-enemies level))
    (add-time-delay enemy time)))

(provide 'roguel-ike-level)
;;; roguel-ike-level.el ends here
