;;; stats.el --- Statistics system

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
;; The entity's statistics are contained in a stat class.
;; This class is a set of slots.

;;; Code:

(require 'cl-generic)
(require 'eieio)
(require 'roguel-ike/stats/slot)

(defclass rlk--stats ()
  ((slots :initarg :slots
          :type list
          :protection :private
          :documentation "An associated list whose keys are slot names and values are slots."))
   "Entity's statistics.")

(cl-defmethod get-slot ((self rlk--stats) slot)
  "Return the slot named SLOT."
  (cdr (assoc slot (oref self slots))))

(cl-defmethod get-slot-names ((self rlk--stats))
  "Return all the slot's names."
  (mapcar (lambda (slot-cons)
            (car slot-cons))
          (oref self slots)))

(provide 'roguel-ike/stats)

;;; stats.el ends here
