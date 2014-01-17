;;; roguel-ike-math.el --- Math utilities for roguel-ike

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
;; Defines math data structures and functions

;;; Code:

(require 'eieio)

;;;;;;;;;;;
;; Point ;;
;;;;;;;;;;;

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

(defmethod add ((point rlk--math-point) vector)
  "Add VECTOR coordinates to POINT coordinates."
  (rlk--math-point "Point"
         :x (+ (get-x point) (get-x vector))
         :y (+ (get-y point) (get-y vector))))

(defmethod subtract ((point rlk--math-point) vector)
  "Subtract VECTOR coordinates to POINT coordinates."
  (rlk--math-point "Point"
         :x (- (get-x point) (get-x vector))
         :y (- (get-y point) (get-y vector))))

(defmethod multiply ((point rlk--math-point) factor)
  "Multiply POINT coordinates by FACTOR."
  (rlk--math-point "Point"
         :x (* (get-x point) factor)
         :y (* (get-y point) factor)))


(defmethod get-distance ((point1 rlk--math-point) point2)
  "Compute destance between POINT1 and POINT2.
POINT1 and POINT2 are conses of the form (x . y)"
  (let ((dx (- (get-x point1) (get-x point2)))
        (dy (- (get-y point1) (get-y point2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;;;;;;;;;;
;; Line ;;
;;;;;;;;;;

(defclass rlk--math-line ()
  ((points :initarg :points
           :type list
           :reader get-points
           :protection :private
           :documentation "Points defining the line."))
  "Represent a line defined by two points.")

(defmethod get-point1 ((line rlk--math-line))
  "Return the first point"
  (car (get-points line)))

(defmethod get-x1 ((line rlk--math-line))
  "Return the absciss of the first point."
  (get-x (get-point1 line)))

(defmethod get-y1 ((line rlk--math-line))
  "Return the ordinate of the first point."
  (get-y (get-point1 line)))

(defmethod get-point2 ((line rlk--math-line))
  "Return the first point"
  (cadr (get-points line)))

(defmethod get-x2 ((line rlk--math-line))
  "Return the absciss of the second point."
  (get-x (get-point2 line)))

(defmethod get-y2 ((line rlk--math-line))
  "Return the ordinate of the second point."
  (get-y (get-point2 line)))

(defmethod get-slope ((line rlk--math-line))
  "Return the slope of the line."
  (/ (- (get-y2 line) (get-y1 line))
     (- (get-x2 line) (get-x1 line))))

(defmethod get-anti-slope ((line rlk--math-line))
  "Return the anti slope of the line."
  (/ (- (get-x2 line) (get-x1 line))
     (- (get-y2 line) (get-y1 line))))

(defmethod get-x-from-y ((line rlk--math-line) y)
  "For a given Y, return the corresponding x value.
Raise error is the line is horizontal."
  (let ((x1 (get-x1 line))
        (y1 (get-y1 line))
        (y2 (get-y2 line)))
    (when (eql y1 y2)
      (error "Cannot get x from y on an horizontal line"))
    (+ x1 (* (get-anti-slope line) (- y y1)))))

(defmethod get-y-from-x ((line rlk--math-line) x)
  "For a given X, return the corresponding y value.
Raise error is the line is vetical."
  (let ((x1 (get-x1 line))
        (y1 (get-y1 line))
        (x2 (get-x2 line)))
    (when (eql x1 x2)
      (error "Cannot get y from x on an vertical line"))
    (+ y1 (* (get-slope line) (- x x1)))))

(provide 'roguel-ike-math)

;;; roguel-ike-math.el ends here
