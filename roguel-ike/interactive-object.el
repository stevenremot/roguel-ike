;;; interactive-object.el --- Level's interactive objects

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
(require 'cl-generic)
(require 'roguel-ike/level)
(require 'roguel-ike/level/cell/object)

(defclass rlk--interactive-object (rlk--level-cell-object)
  ()
  "Base class for interactive objects."
  :abstract t)

(cl-defmethod get-layer ((self rlk--interactive-object))
  "See rlk--level-cell-object."
  2)

(cl-defmethod do-action ((self rlk--interactive-object) hero action)
  "Do the ACTION when the HERO interacts with it.
Return t when the action was successfull, nil otherwise."
  (error "Method do-action must be overriden"))

(provide 'roguel-ike/interactive-object)

;;; interactive-object.el ends here
