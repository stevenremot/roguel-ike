;;; race.el --- roguel-ike race system

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
;; Each entity belongs to a race.
;; Races defines the way entities evolves, and their intrinsics skills

;;; Code:

(require 'eieio)
(require 'roguel-ike/skill)

;;;;;;;;;;;;;;;;;
;; Race classe ;;
;;;;;;;;;;;;;;;;;

(defclass rlk--race ()
  ((type :initarg :type
         :type symbol
         :reader get-type
         :protection :private
         :documentation "The identifier of the race.")
   (name :initarg :name
         :type string
         :reader get-name
         :protection :private
         :documentation "The name to use when an entity belonging to this race is referred.")
   (base-stats :initarg :base-stats
               :type list
               :reader get-base-stats
               :protection :private
               :documentation "The statistics of a new entity with this race.")
   (stats-evolution :initarg :stats-evolution
                    :type list
                    :reader get-stats-evolution
                    :protection :private
                    :documentation "The points added to each stat slot when it gains a level.")
   (skills :initarg :skills
           :type list
           :protection :private
           :documentation "A list of symbol's skills.

An entity of this race can potentially use any of these skills."))
  "A entity race.")

(defmethod get-base-stat-slot ((self rlk--race) slot)
  "Return the base stat for the slot SLOT."
  (plist-get (get-base-stats self) slot))

(defmethod get-stat-slot-evolution ((self rlk--race) slot)
  "Return the evolution of the stat slot SLOT."
  (plist-get (get-stats-evolution self) slot))

(defmethod get-skills ((self rlk--race))
  "Return the stat's skills as rlk--skill objects."
  (mapcar (lambda (id)
            (rlk--skill-get-skill id))
          (oref self skills)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Races registering system ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rlk--races '()
  "All the available races.")

(defun rlk--defrace (type name base-stats stats-evolution skills)
  "Define a new race and register it as an available race.
TYPE is the identifier of the race.
NAME is the name used when an entity belonging to this race is referred
BASE-STATS is the statistics of a newly created entity in this race.
STATS-EVOLUTION define the way each stat evolve.
SKILLS is the race's skills.

Example:
  (rlk--defrace :coder
                \"The coder\"
                (list
                     :health 5
                     :stamina 0
                     :strength 2
                     :constitution 2
                     :speed 6
                     :spirit 0)
                (list
                     :health 2
                     :stamina 1
                     :strength 1
                     :constitution 1
                     :speed 3
                     :spirit 0)
                '(:define-macro))"
  (add-to-list 'rlk--races (rlk--race name
                                      :type type
                                      :name name
                                      :base-stats base-stats
                                      :stats-evolution stats-evolution
                                      :skills skills)))

(defun rlk--race-get-race (type)
  "Return the registered race corresponding to the type TYPE.
If there is no such race, return nil."
  (catch 'race
    (dolist (race rlk--races)
      (when (equal (get-type race) type)
        (throw 'race race)))
    nil))

(provide 'roguel-ike/race)

;;; race.el ends here
