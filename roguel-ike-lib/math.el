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

(provide 'roguel-ike-lib/math)

;;; math.el ends here
