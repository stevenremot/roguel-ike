;;; object.el --- Object contained in a cell

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
;; Here is the base class for objects present on a level cell.

;;; Code:
(require 'eieio)
(require 'roguel-ike/level/cell)
(require 'roguel-ike/level)

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

(provide 'roguel-ike/level/cell/object)

;;; object.el ends here
