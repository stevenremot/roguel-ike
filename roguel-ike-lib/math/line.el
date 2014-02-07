;;; line.el --- A two-dimensional line

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
;; Define a two-dimensional line structure, defined by two points.
;; It is intended to be used in geometrical computations.

;;; Code:
(require 'roguel-ike-lib/math/point)

(defclass roguel-ike-math-line ()
  ((points :initarg :points
           :type list
           :reader get-points
           :protection :private
           :documentation "Points defining the line."))
  "Represent a line defined by two points.

This class is intended to be used for geometrical compuations
implying line, finite or infinite.")

(defmethod get-point1 ((self roguel-ike-math-line))
  "Return the first point"
  (car (get-points self)))

(defmethod get-x1 ((self roguel-ike-math-line))
  "Return the absciss of the first point."
  (get-x (get-point1 self)))

(defmethod get-y1 ((self roguel-ike-math-line))
  "Return the ordinate of the first point."
  (get-y (get-point1 self)))

(defmethod get-point2 (selfroguel-ike-math-line)
  "Return the first point"
  (cadr (get-points self)))

(defmethod get-x2 ((self roguel-ike-math-line))
  "Return the absciss of the second point."
  (get-x (get-point2 self)))

(defmethod get-y2 ((self roguel-ike-math-line))
  "Return the ordinate of the second point."
  (get-y (get-point2 self)))

(defmethod get-slope ((self roguel-ike-math-line))
  "Return the slope of the line."
  (/ (- (get-y2 self) (get-y1 self))
     (- (get-x2 self) (get-x1 self))))

(defmethod get-anti-slope ((self roguel-ike-math-line))
  "Return the anti slope of the line."
  (/ (- (get-x2 self) (get-x1 self))
     (- (get-y2 self) (get-y1 self))))

(defmethod get-x-from-y ((self roguel-ike-math-line) y)
  "For a given Y, return the corresponding x value.
Raise error is the line is horizontal."
  (let ((x1 (get-x1 self))
        (y1 (get-y1 self))
        (y2 (get-y2 self)))
    (when (eql y1 y2)
      (error "Cannot get x from y on an horizontal line"))
    (+ x1 (* (get-anti-slope self) (- y y1)))))

(defmethod get-y-from-x ((self roguel-ike-math-line) x)
  "For a given X, return the corresponding y value.
Raise error is the line is vetical."
  (let ((x1 (get-x1 self))
        (y1 (get-y1 self))
        (x2 (get-x2 self)))
    (when (eql x1 x2)
      (error "Cannot get y from x on an vertical line"))
    (+ y1 (* (get-slope self) (- x x1)))))


(provide 'roguel-ike-lib/math/line)
;;; line.el ends here
