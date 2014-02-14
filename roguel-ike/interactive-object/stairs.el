;;; stairs.el --- An object to go from one level to another

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
;;

;;; Code:
(require 'roguel-ike/interactive-object)

(defclass rlk--interactive-object-stairs (rlk--interactive-object)
  ((direction :initarg :direction
              :reader get-direction
              :type symbol
              :protection :private
              :documentation ":up or :down.")
   (callback :initarg :callback
             :type function
             :protection :private
             :documentation "Function to call when an entity walks on the stair.

The function must take an entity as parameter."))
  "This object allow the player to travel from one level to another.")

(defmethod get-type ((self rlk--interactive-object-stairs))
  (cond ((eq :up (get-direction self)) :stairs-up)
        (t :stairs-down)))

(defmethod accept-other-object-p ((self rlk--interactive-object-stairs))
  t)

(defmethod block-light-p ((self rlk--interactive-object-stairs))
  nil)

(defmethod do-action ((self rlk--interactive-object-stairs) entity action)
  (funcall (oref self callback) entity))

(provide 'roguel-ike/interactive-object/stairs)

;;; stairs.el ends here
