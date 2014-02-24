;;; slot.el --- A statistics slot

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
;; The statstics slot are composed of a current value and a maximum value.

;;; Code:
(require 'roguel-ike-lib/dispatcher)

(defclass rlk--stats-slot ()
  ((base-value :initarg :base-value
              :type integer
              :reader get-base-value
              :writer set-base-value
              :protection :private
              :documentation "The base value of the statistic.")
   (current-value :type integer
                  :protection :private
                  :documentation "The current value of the statistic.
Cannot be out of the range 0 - base-value.")
   (experience :initarg :experience
               :initform 0
               :type integer
               :reader get-experience
               :protection :private
               :documentation "The current experience for this slot.

When experience reaches a certain point, the max value increases.")
   (experience-rate :initarg :experience-rate
                    :type number
                    :reader get-experience-rate
                    :protection :private
                    :documentation "The maximum value will be incremented when experience reached experience-rate * base-value.")
   (dispatcher :type roguel-ike-dispatcher
               :reader get-dispatcher
               :protection :private
               :documentation "The event dispatcher for the slot.

Currently, dispatched events are:
- :current-value-changed OLD-VALUE NEW-VALUE"))
  "Statistic slot.
Handle maximum value and current value.")

(defmethod initialize-instance :after ((self rlk--stats-slot) slots)
  "Initialize dispatcher."
  (oset self dispatcher (roguel-ike-dispatcher "Slot dispatcher")))

(defmethod get-current-value ((self rlk--stats-slot))
  "Return the current slot value.

Set it to base-value if not set yet."
  (unless (slot-boundp self 'current-value)
    (oset self current-value (get-base-value self)))
  (oref self current-value))

(defmethod set-current-value ((self rlk--stats-slot) current-value)
  "Set the current slot value.

Restrain it to be positive."
  (let ((old-value (get-current-value self))
        (new-value (max 0 current-value)))
    (oset self current-value new-value)
    (dispatch (get-dispatcher self) :current-value-changed old-value new-value)))

(defmethod add-experience ((self rlk--stats-slot) experience)
  "Add EXPERIENCE to current experience points."
  (let ((threshold (* (get-experience-rate self) (get-base-value self))))
    (oset self experience (+ (get-experience self) experience))

    (when (>= (get-experience self) threshold)
      (oset self experience (- (get-experience self) threshold))
      (set-base-value self (1+ (get-base-value self))))))

(provide 'roguel-ike/stats/slot)

;;; slot.el ends here
