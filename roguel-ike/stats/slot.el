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
(require 'eieio)

(defclass rlk--stats-slot ()
  ((max-value :initarg :max-value
              :type integer
              :reader get-max-value
              :writer set-max-value
              :protection :private
              :documentation "The maximum value of the statistic.")
   (current-value :type integer
                  :protection :private
                  :documentation "The current value of the statistic.
Cannot be out of the range 0 - max-value.")
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
                    :documentation "The maximum value will be incremented when experience reached experience-rate * max-value."))
  "Statistic slot.
Handle maximum value and current value.")

(defmethod get-current-value ((self rlk--stats-slot))
  "Return the current slot value.
Set it to max-value if not set yet."
  (unless (slot-boundp self 'current-value)
    (set-current-value self (get-max-value self)))
  (oref self current-value))

(defmethod set-current-value ((self rlk--stats-slot) current-value)
  "Set the current slot value.
Restrain it to the range 0 - max-value."
  (oset self current-value
        (cond ((< current-value 0)
               0)
              ((> current-value (get-max-value self))
               (get-max-value self))
              (t
               current-value))))

(defmethod add-experience ((self rlk--stats-slot) experience)
  "Add EXPERIENCE to current experience points."
  (let ((threshold (* (get-experience-rate self) (get-max-value self))))
    (oset self experience (+ (get-experience self) experience))

    (when (>= (get-experience self) threshold)
      (oset self experience (- (get-experience self) threshold))
      (set-max-value self (1+ (get-max-value self))))))

(provide 'roguel-ike/stats/slot)

;;; slot.el ends here
