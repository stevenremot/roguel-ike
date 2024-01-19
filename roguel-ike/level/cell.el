;;; cell.el --- Level cell

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
;; Represent a cell in a level

;;; Code:
(require 'cl-generic)
(require 'eieio)

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

(cl-defmethod get-visible-type ((self rlk--level-cell))
  "Return the visible type of the cell."
  (if (is-visited-p self)
      (get-type self)
    :void))

(cl-defmethod is-container-p ((self rlk--level-cell))
  "Return t if the cell can contain objects, nil otherwise."
  nil)

(cl-defmethod is-accessible-p ((self rlk--level-cell))
  "Return t if the cell can be the destination of an entity, nil otherwise."
  nil)

(cl-defmethod block-light-p ((self rlk--level-cell))
  "Return t if the cell blocks the light, nil otherwise."
  t)

(cl-defmethod set-lit :after ((self rlk--level-cell) lit)
  "When the cell has been lit, set it as visited."
  (when lit
    (set-visited self t)))

(cl-defgeneric has-entity-p (cell)
  "Return t when a container entity has an entity standing on it.")

(cl-defgeneric get-entity (cell)
  "Return the entity standing on the container cell.")

(provide 'roguel-ike/level/cell)

;;; cell.el ends here
