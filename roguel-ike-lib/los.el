;;; los.el --- Line of sight calculation

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
;; Implement a line of sight algorithm.

;; A line of sight algorithm determines if some point A is visible
;; from point B in a level.  To do this, the algorithm simply draw
;; the line going from B to A, and checks if there isn't any cell
;; blocking light on this line.

;; The drawing of the line is done using a simple Bresenham algorithm.

;; This method is the most basic, and should be avoided for realistic line
;; of sight computations.  It is OK in most cases though.

;;; Code:
(require 'roguel-ike-lib/cell)
(require 'roguel-ike-lib/level)
(require 'roguel-ike-lib/math/point)

(defun roguel-ike-los-can-see-p (origin target level)
  "Return t if ORIGIN can see TARGET in LEVEL.

ORIGIN and TARGET are conses of the form (x . y) or points.
LEVEL is a roguel-ike-level."
  (unless (roguel-ike-math-point-child-p origin)
    (setq origin (roguel-ike-math-point "LOS origin"
                                  :x (car origin)
                                  :y (cdr origin))))
  (unless (roguel-ike-math-point-child-p target)
    (setq target (roguel-ike-math-point "LOS target"
                                  :x (car target)
                                  :y (cdr target))))

  (let* ((x1 (get-x origin))
         (y1 (get-y origin))
         (x2 (get-x target))
         (y2 (get-y target))
         (main-direction (if (> (abs (- y2 y1)) (abs (- x2 x1)))
                             (if (> y2 y1)
                                 (roguel-ike-math-point "Direction" :x 0 :y 1)
                               (roguel-ike-math-point "Direction" :x 0 :y -1))
                           (if (> x2 x1)
                               (roguel-ike-math-point "Direction" :x 1 :y 0)
                             (roguel-ike-math-point "Direction" :x -1 :y 0))))
         (secondary-direction (if (<= (abs (- y2 y1)) (abs (- x2 x1)))
                             (if (> y2 y1)
                                 (roguel-ike-math-point "Direction" :x 0 :y 1)
                               (roguel-ike-math-point "Direction" :x 0 :y -1))
                           (if (> x2 x1)
                               (roguel-ike-math-point "Direction" :x 1 :y 0)
                             (roguel-ike-math-point "Direction" :x -1 :y 0))))
         (slope (abs (if (<= (abs (- y2 y1)) (abs (- x2 x1)))
                         (if (= x2 x1)
                             0.0
                           (/ (float (- y2 y1)) (float (- x2 x1))))
                       (if (= y2 y1)
                           0.0
                         (/ (float (- x2 x1)) (float (- y2 y1)))))))
         (current-slope 0.0)
         (current-point origin)
         (cell nil))
    (catch 'can-see
      (while (not (= (apply-scalar current-point main-direction)
                     (apply-scalar target main-direction)))
        (setq current-point (add current-point main-direction)
              current-slope (+ current-slope slope))
        (if (>= current-slope 1)
          (setq current-point (add current-point secondary-direction)
                current-slope (- current-slope 1)))
        (setq cell (get-cell-at level (get-x current-point) (get-y current-point)))
        (when (block-light-p cell)
          (throw 'can-see nil)))
      t)))

(provide 'roguel-ike-lib/los)

;;; los.el ends here
