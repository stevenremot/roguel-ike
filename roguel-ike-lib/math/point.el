;;; point.el --- Point data structure

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
;; Define a basic data structure for a two-dimensional point.
;; This data structure is intended to be used in geometrical
;; computation.

;;; Code:
(require 'eieio)

(defclass roguel-ike-math-point ()
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
  "A two-dimensionnal point.

This class implements different geometrical methods, and is therefore
intended to be used for geometrical computations implying points or
vectors.")

(defmethod add ((self roguel-ike-math-point) vector)
  "Return the sum of SELF and VECTOR."
  (roguel-ike-math-point "Point"
         :x (+ (get-x self) (get-x vector))
         :y (+ (get-y self) (get-y vector))))

(defmethod subtract ((self roguel-ike-math-point) vector)
  "Return the subtraction of SELF and VECTOR."
  (roguel-ike-math-point "Point"
         :x (- (get-x self) (get-x vector))
         :y (- (get-y self) (get-y vector))))

(defmethod multiply ((self roguel-ike-math-point) factor)
  "Return SELF multiplyed by FACTOR.

FACTOR is a number."
  (roguel-ike-math-point "Point"
         :x (* (get-x self) factor)
         :y (* (get-y self) factor)))

(defmethod apply-scalar ((self roguel-ike-math-point) vector)
  "Compute the scalar product for two vectors."
  (+ (* (get-x self) (get-x vector))
     (* (get-y self) (get-y vector))))

(defmethod equal-p ((self roguel-ike-math-point) point)
  "Return t if SELF and POINT has same coordinates."
  (and (= (get-x self) (get-x point))
       (= (get-y self) (get-y point))))


(defmethod get-distance ((self roguel-ike-math-point) point)
  "Compute euclidian distance between SELF and POINT."
  (let ((dx (- (get-x self) (get-x point)))
        (dy (- (get-y self) (get-y point))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(provide 'roguel-ike-lib/math/point)
;;; point.el ends here
