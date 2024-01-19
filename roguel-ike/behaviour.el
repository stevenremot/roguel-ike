;;; behaviour.el --- Entities' behaviour

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
(require 'cl-generic)
(require 'roguel-ike/entity)

(defclass rlk--behaviour ()
  ((entity :reader get-entity
           :writer set-entity
           :type rlk--entity
           :protection :private
           :documentation "THe tntiy the current behaviour controls."))
  "Base class for behaviour objects."
  :abstract t)

(cl-defmethod do-action ((self rlk--behaviour) callback)
  "Decide which action should be done now.
Must call callback with the number of turns the action takes."
  (error "Method do-action for behaviour must be overriden"))

(cl-defmethod is-manual-p ((self rlk--behaviour))
  "Return t if the behaviour is manual, nil otherwise."
  nil)

(provide 'roguel-ike/behaviour)

;;; behaviour.el ends here
