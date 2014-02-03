;;; point.el --- 2-dimensional point

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

(defclass rlk--math-point ()
  ((x :initarg :x
      :initform 0
      :type number
      :reader get-x
      :writer set-x
      :protection :private
      :documentation "Absciss.")
   (y :initarg :y
      :initform 0
      :type number
      :reader get-y
      :writer set-y
      :protection :private
      :documentation "Ordinate."))
  "A two-dimensionnal point.")

(defmethod add ((self rlk--math-point) vector)
  "Add VECTOR coordinates to SELF coordinates."
  (rlk--math-point "Point"
         :x (+ (get-x self) (get-x vector))
         :y (+ (get-y self) (get-y vector))))

(defmethod subtract ((self rlk--math-point) vector)
  "Subtract VECTOR coordinates to SELF coordinates."
  (rlk--math-point "Point"
         :x (- (get-x self) (get-x vector))
         :y (- (get-y self) (get-y vector))))

(defmethod multiply ((self rlk--math-point) factor)
  "Multiply SELF coordinates by FACTOR."
  (rlk--math-point "Point"
         :x (* (get-x self) factor)
         :y (* (get-y self) factor)))

(defmethod apply-scalar ((self rlk--math-point) vector)
  "Compute the scalar product for two vectors."
  (+ (* (get-x self) (get-x vector))
     (* (get-y self) (get-y vector))))

(defmethod equal-p ((self rlk--math-point) point)
  "Return t if SELF and POINT has same coordinates."
  (and (= (get-x self) (get-x point))
       (= (get-y self) (get-y point))))


(defmethod get-distance ((point1 rlk--math-point) point2)
  "Compute destance between POINT1 and POINT2.
POINT1 and POINT2 are conses of the form (x . y)"
  (let ((dx (- (get-x point1) (get-x point2)))
        (dy (- (get-y point1) (get-y point2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(provide 'roguel-ike/math/point)

;;; point.el ends here
