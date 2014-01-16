;;; roguel-ike-interactive-object.el --- Level's interactive objects

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
;; Defines interactive objects in the level

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)

;;;;;;;;;;;;;;;;
;; Base class ;;
;;;;;;;;;;;;;;;;

(defclass rlk--interactive-object (rlk--level-cell-object)
  ()
  "Base class for interactive objects."
  :abstract t)

(defmethod get-layer ((object rlk--interactive-object))
  "See rlk--level-cell-object."
  2)

(defmethod do-action ((object rlk--interactive-object) hero action)
  "Do the ACTION when the HERO interacts with it.
Return t when the action was successfull, nil otherwise."
  (error "Method do-action must be overriden"))

;;;;;;;;;;
;; Door ;;
;;;;;;;;;;

(defclass rlk--interactive-object-door (rlk--interactive-object)
  ((opened :initform nil
          :type boolean
          :reader is-opened-p
          :protection :private
          :documentation "Tell whether the door is opened or not."))
  "A door that can be opened and closed.
An entity cannot pass when door is closed.")

(defmethod get-type ((door rlk--interactive-object-door))
  "See rlk--level-cell-object."
  (if (is-opened-p door)
      :door-opened
    :door-closed))

(defmethod accept-other-object-p ((door rlk--interactive-object-door))
  "See rlk--level-cell-object."
  (is-opened-p door))

(defmethod do-action ((door rlk--interactive-object-door) hero action)
  "Close / open the door.
When the action is :open and the door is closed, open it.
When the action is :close and the door is open, close it."
  (cond
   ((and (is-opened-p door) (equal action :close))
    (oset door opened nil)
    t)
   ((and (not (is-opened-p door)) (equal action :open))
    (oset door opened t)
    t)
   (t
    nil)))

(provide 'roguel-ike-interactive-object)

;;; roguel-ike-interactive-object.el ends here
