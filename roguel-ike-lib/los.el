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
(require 'roguel-ike-lib/math)

(defun roguel-ike-los-can-see-p (origin target level)
  "Return t if ORIGIN can see TARGET in LEVEL.

ORIGIN and TARGET are conses of the form (x . y).
LEVEL is a roguel-ike-level."
  (let* ((x1 (car origin))
         (y1 (cdr origin))
         (x2 (car target))
         (y2 (cdr target))
         (main-direction (if (> (abs (- y2 y1)) (abs (- x2 x1)))
                             (if (> y2 y1)
                                 (cons 0 1)
                               (cons 0 -1))
                           (if (> x2 x1)
                               (cons 1 0)
                             (cons -1 0))))
         (secondary-direction (if (<= (abs (- y2 y1)) (abs (- x2 x1)))
                                  (if (> y2 y1)
                                      (cons 0 1)
                                    (cons 0 -1))
                                (if (> x2 x1)
                                    (cons 1 0)
                                  (cons -1 0))))
         (slope (abs (if (<= (abs (- y2 y1)) (abs (- x2 x1)))
                         (if (= x2 x1)
                             0.0
                           (/ (float (- y2 y1)) (float (- x2 x1))))
                       (if (= y2 y1)
                           0.0
                         (/ (float (- x2 x1)) (float (- y2 y1)))))))
         (current-slope 0.0)
         (current-point (cons x1 y1))
         (cell nil))

    (catch 'can-see
      (while (not (= (roguel-ike-math-apply-scalar current-point main-direction)
                     (roguel-ike-math-apply-scalar target main-direction)))

        (setf (car current-point) (+ (car current-point) (car main-direction))
              (cdr current-point) (+ (cdr current-point) (cdr main-direction))
              current-slope (+ current-slope slope))

        (if (>= current-slope 1)
            (setf (car current-point) (+ (car current-point) (car secondary-direction))
                  (cdr current-point) (+ (cdr current-point) (cdr secondary-direction))
                  current-slope (1- current-slope)))

        (setq cell (get-cell-at level (car current-point) (cdr current-point)))
        (when (block-light-p cell)
          (throw 'can-see nil)))
      t)))

(provide 'roguel-ike-lib/los)
;;; los.el ends here
