;;; ground.el --- Cell representing ground

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
;; Special cell that can contain object.

;;; Code:
(require 'cl-generic)
(require 'roguel-ike/level/cell)
(require 'roguel-ike/level/cell/object)

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

(cl-defmethod get-visible-type ((self rlk--level-cell-ground))
  "Return the visible type of the cell."
  (let ((highest-object (get-highest-layer-object self)))
    (if (and (is-lit-p self)
             highest-object)
        (get-type highest-object)
      (call-next-method))))

(cl-defmethod is-container-p ((self rlk--level-cell-ground))
  "Return t if the cell can contain objects, nil otherwise."
  t)

(cl-defmethod add-object ((self rlk--level-cell-ground) object)
  "Add OBJECT into CELL's objects if it is not already in."
  (let ((objects (get-objects self)))
    (set-objects self (add-to-list 'objects object))))

(cl-defmethod remove-object ((self rlk--level-cell-ground) object)
  "Remove OBJECT from CELL's objects if it is in."
  (set-objects self (delete object (get-objects self))))

(cl-defmethod get-entity ((self rlk--level-cell-ground))
  "Return the entity on the CELL if there is currently one.
Return nil otherwise."
  (catch 'entity
    (dolist (object (get-objects self))
      (when (is-entity-p object)
        (throw 'entity object)))
    (throw 'entity nil)))

(cl-defmethod set-entity ((self rlk--level-cell-ground) entity)
  "Set the ENTITY standing on the CELL.
If there is already an entity on it, it will be removed."
  (let ((old-entity (get-entity self)))
    (when old-entity
      (remove-object self old-entity))
    (when entity
      (add-object self entity))))

(cl-defmethod has-entity-p ((self rlk--level-cell-ground))
  "Return `t' if the cell contains an entity, nil otherwise"
  (rlk--level-cell-object-child-p (get-entity self)))

(cl-defmethod get-highest-layer-object ((self rlk--level-cell-ground))
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

(cl-defmethod is-accessible-p ((self rlk--level-cell-ground))
  "Return t if cell can accept a new object, nil otherwise."
  (catch 'accessible
    (dolist (object (get-objects self))
      (unless (accept-other-object-p object)
        (throw 'accessible nil)))
    t))

(cl-defmethod block-light-p ((self rlk--level-cell-ground))
  "See rlk--level-cell."
  (catch 'block-light
    (dolist (object (get-objects self))
      (when (block-light-p object)
        (throw 'block-light t)))
    nil))

(provide 'roguel-ike/level/cell/ground)

;;; ground.el ends here
