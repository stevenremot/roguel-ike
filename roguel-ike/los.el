;;; los.el --- Line of sight system

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
;; Defines field of view calculation system

;; The field of view is currently computed using recursive shadowcasting.
;; See http://www.roguebasin.com/index.php?title=FOV_using_recursive_shadowcasting
;; to get a introduction to the method.

;;; Commentary:
;;

;;; Code:
(require 'roguel-ike/math/point)
(require 'roguel-ike/math/line)
(require 'roguel-ike/level)
(require 'roguel-ike/level/cell)

(defun rlk--los-can-see-p (origin target level)
  "Return t if ORIGIN can see TARGET in LEVEL.

ORIGIN and TARGET are conses of the form (x . y) or points.
LEVEL is a rlk--level."
  (unless (rlk--math-point-child-p origin)
    (setq origin (rlk--math-point "LOS origin"
                                  :x (car origin)
                                  :y (cdr origin))))
  (unless (rlk--math-point-child-p target)
    (setq target (rlk--math-point "LOS target"
                                  :x (car target)
                                  :y (cdr target))))

  (let* ((x1 (get-x origin))
         (y1 (get-y origin))
         (x2 (get-x target))
         (y2 (get-y target))
         (main-direction (if (> (abs (- y2 y1)) (abs (- x2 x1)))
                             (if (> y2 y1)
                                 (rlk--math-point "Direction" :x 0 :y 1)
                               (rlk--math-point "Direction" :x 0 :y -1))
                           (if (> x2 x1)
                               (rlk--math-point "Direction" :x 1 :y 0)
                             (rlk--math-point "Direction" :x -1 :y 0))))
         (secondary-direction (if (<= (abs (- y2 y1)) (abs (- x2 x1)))
                             (if (> y2 y1)
                                 (rlk--math-point "Direction" :x 0 :y 1)
                               (rlk--math-point "Direction" :x 0 :y -1))
                           (if (> x2 x1)
                               (rlk--math-point "Direction" :x 1 :y 0)
                             (rlk--math-point "Direction" :x -1 :y 0))))
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
      (while (not (and (= (get-x current-point) x2)
                       (= (get-y current-point) y2)))
        (setq current-point (add current-point main-direction)
              current-slope (+ current-slope slope))
        (if (>= current-slope 1)
          (setq current-point (add current-point secondary-direction)
                current-slope (- current-slope 1)))
        (setq cell (get-cell-at level (get-x current-point) (get-y current-point)))
        (when (block-light-p cell)
          (throw 'can-see nil)))
      t)))

(provide 'roguel-ike/los)

;;; los.el ends here
