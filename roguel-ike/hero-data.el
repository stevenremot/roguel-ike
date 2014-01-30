;;; hero-data.el --- Data representing a hero

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
(require 'eieio)

(defclass rlk--hero-data ()
  ((name :initarg :name
         :type string
         :reader get-name
         :writer set-name
         :protection :private
         :documentation "The hero's name. Identify the save slot.")
   (race :initarg :race
         :type symbol
         :reader get-race
         :writer set-race
         :protection :private
         :documentation "The hero's race.")
   (stats :initarg :stats
          :type list
          :reader get-stats
          :writer set-stats
          :protection :private
          :documentation "The hero's maximum stats."))
  "Contain all the persistent data describing a hero.

A hero data can be converted from / to a list in the form :

'(\"Name\"
  :race
  (:health 10
   :stamina 5
   ...))")

(defmethod to-list ((self rlk--hero-data))
  "Convert the data to a list representation."
  (list (get-name self)
        (get-race self)
        (get-stats self)))

(defun rlk--hero-data-create-from-list (hero-list)
  "Create a hero data from its list representation HERO-LIST."
  (rlk--hero-data "Hero data"
                  :name (nth 0 hero-list)
                  :race (nth 1 hero-list)
                  :stats (nth 2 hero-list)))

(provide 'roguel-ike/hero-data)

;;; hero-data.el ends here
