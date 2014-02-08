;;; math.el --- Math utilities

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
;; Define some math functions to do geometrical computations.

;; These functions manipulates two data structures:

;; * A two-dimensional point or vector is simply a cons in the form
;;   `(x . y)'

;; * A two-dimensional line is a cons of two points.

;;; Code:

(defun roguel-ike-math-apply-scalar (vector1 vector2)
  "Return the scalar product between VECTOR1 and VECTOR2."
  (+ (* (car vector1) (car vector2))
     (* (cdr vector1) (cdr vector2))))

(defun roguel-ike-math-get-distance (point1 point2)
  "Return the euclidian distance between POINT1 and POINT2."
  (let ((dx (- (car point1) (car point2)))
        (dy (- (cdr point1) (cdr point2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun roguel-ike-math-get-slope (line)
  "Return LINE's slope."
  (/ (float (- (cddr line) (cdar line)))
     (float (- (cadr line) (caar line)))))

(defun roguel-ike-math-get-anti-slope (line)
  "Return LINE's antislope."
  (/ (float (- (cadr line) (caar line)))
     (float (- (cddr line) (cdar line)))))

(defun roguel-ike-math-get-x-from-y (line y)
  "Return the absciss of the point in LINE with Y as ordinate."
  (let ((x1 (float (caar line)))
        (y1 (cdar line))
        (y2 (cddr line)))
    (when (= y1 y2)
      (error "Cannot get x from y on a horizontal line"))
    (+ x1 (* (roguel-ike-math-get-anti-slope line) (float (- y y1))))))

(defun roguel-ike-math-get-y-from-x (line x)
  "Return the ordinate of the point in LINE with X as absciss."
  (let ((x1 (caar line))
        (y1 (float (cdar line)))
        (x2 (cadr line)))
    (when (= x1 x2)
      (error "Cannot get y from x on a vertical line"))
    (+ y1 (* (roguel-ike-math-get-slope line) (float (- x x1))))))

(provide 'roguel-ike-lib/math)

;;; math.el ends here
