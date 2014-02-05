;;; fov.el --- Field of view system for roguel-ike

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

;;; Code:
(require 'roguel-ike/level)
(require 'roguel-ike/level/cell)
(require 'roguel-ike/math/line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coordinates transformer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rlk--fov-coordinate-transformer ()
  ((row-direction :initarg :row-direction
                  :type rlk--math-point
                  :reader get-row-direction
                  :protection :private
                  :documentation "Direction of the rows.")
   (column-direction :initarg :column-direction
                     :type rlk--math-point
                     :reader get-column-direction
                     :protection :private
                     :documentation "Direction of the columns."))
  "Utility used in the iterator.
It knows how to transform initial coordinates to go to the next row / next cell in the row.")

(defmethod get-next-cell ((transformer rlk--fov-coordinate-transformer) pos)
  "Return the next cell's position in the row.

For the sake of optimization, POS is recycled to create the next position."
  (let ((row-direction (get-row-direction transformer)))
    (set-x pos (+ (get-x pos) (get-x row-direction)))
    (set-y pos (+ (get-y pos) (get-y row-direction)))
    pos))

(defmethod get-cell-from-line ((transformer rlk--fov-coordinate-transformer) line depth)
  "Return the cell intersecting the LINE at the given DEPTH."
  (let* ((column-direction (get-column-direction transformer))
         (x1 (get-x1 line))
         (y1 (get-y1 line))
         (dx (get-x column-direction))
         (dy (get-y column-direction)))
    (if (eql dx 0)
        (let* ((y (+ y1 (* dy depth)))
               (x (round (get-x-from-y line y))))
          (rlk--math-point "Point" :x x :y y))
      (let* ((x (+ x1 (* dx depth)))
             (y (round (get-y-from-x line x))))
        (rlk--math-point "Point" :x x :y y)))))

(defmethod create-start-line ((transformer rlk--fov-coordinate-transformer) origin start-cell)
  "Create a line going from ORIGIN to an start-CELL corner.

This line is intended to be used as an start limit for the shadowcasting algorithm."
  (rlk--math-line "Line"
                  :points (list origin
                                (add start-cell (multiply (add (get-column-direction transformer)
                                                                  (get-row-direction transformer))
                                                             0.5)))))

(defmethod create-end-line ((transformer rlk--fov-coordinate-transformer) origin end-cell)
  "Create a line going from ORIGIN to an END-CELL corner.

This line is intended to be used as an end limit for the shadowcasting algorithm."
  (rlk--math-line "Line"
                  :points (list origin
                                (subtract end-cell (multiply (add (get-column-direction transformer)
                                                                  (get-row-direction transformer))
                                                             0.5)))))

(defmethod end-reached-p ((self rlk--fov-coordinate-transformer) origin current-cell-pos last-cell-pos)
  "Return t is the end of the row is reached, nil otherwise."
  (let ((row-direction (get-row-direction self)))
    (> (apply-scalar (subtract current-cell-pos origin) row-direction)
       (apply-scalar (subtract last-cell-pos origin) row-direction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shadow casting algorithm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rlk--fov-compute-fov-part  (level transformer start-line end-line radius depth)
  "Compute a part of fov for LEVEL.

Navigate in cells using TRANSFORMER.
Compute fov between START-LINE and END-LINE, limited to the given RADIUS,
starting to the given DEPTH."
  (when (<= depth radius)
    (let ((cell-pos (get-cell-from-line transformer start-line depth))
          (last-cell-pos (get-cell-from-line transformer end-line depth))
          (origin (get-point1 start-line))
          (in-wall t)
          (starting-wall t)
          (end-reached nil))

      ;; Firstly check if the first position is not after the last position
      ;; with the powerful method end-reached-p.  As this method is expensive
      ;; prefer the use of equal-p in the loop now that we are sure cell-pos
      ;; will eventually equal last-cell-pos.
      (unless (end-reached-p transformer origin cell-pos last-cell-pos)
        (while (not end-reached)
          (when (<= (get-distance cell-pos origin) radius)
            (let ((cell (get-cell-at level (get-x cell-pos) (get-y cell-pos))))
              (when cell
                (set-lit cell t)
                (set-visited cell t)

                (if (block-light-p cell)
                    (progn
                      (unless in-wall
                        (setq in-wall t)
                        (unless starting-wall
                          (rlk--fov-compute-fov-part level
                                                     transformer
                                                     start-line
                                                     (create-end-line transformer origin cell-pos)
                                                     radius
                                                     (1+ depth))))
                      (setq start-line (create-start-line transformer origin cell-pos)))
                  (progn
                    (setq in-wall nil
                          starting-wall nil))))))

          (setq end-reached (equal-p cell-pos last-cell-pos)
                cell-pos (get-next-cell transformer cell-pos))))

      (unless in-wall
        (rlk--fov-compute-fov-part level transformer start-line end-line radius (1+ depth))))))

(defconst rlk--fov-octans
  (list
   (list
    (rlk--math-point "Start direction point" :x -1 :y -1)
    (rlk--math-point "End direction point" :x 0 :y -1)
    (rlk--math-point "Row direction" :x 1 :y 0)
    (rlk--math-point "Column direction" :x 0 :y -1))
   (list
    (rlk--math-point "Start direction point" :x 1 :y -1)
    (rlk--math-point "End direction point" :x 0 :y -1)
    (rlk--math-point "Row direction" :x -1 :y 0)
    (rlk--math-point "Column direction" :x 0 :y -1))
   (list
    (rlk--math-point "Start direction point" :x 1 :y -1)
    (rlk--math-point "End direction point" :x 1 :y 0)
    (rlk--math-point "Row direction" :x 0 :y 1)
    (rlk--math-point "Column direction" :x 1 :y 0))
   (list
    (rlk--math-point "Start direction point" :x 1 :y 1)
    (rlk--math-point "End direction point" :x 1 :y 0)
    (rlk--math-point "Row direction" :x 0 :y -1)
    (rlk--math-point "Column direction" :x 1 :y 0))
   (list
    (rlk--math-point "Start direction point" :x 1 :y 1)
    (rlk--math-point "End direction point" :x 0 :y 1)
    (rlk--math-point "Row direction" :x -1 :y 0)
    (rlk--math-point "Column direction" :x 0 :y 1))
   (list
    (rlk--math-point "Start direction point" :x -1 :y 1)
    (rlk--math-point "End direction point" :x 0 :y 1)
    (rlk--math-point "Row direction" :x 1 :y 0)
    (rlk--math-point "Column direction" :x 0 :y 1))
   (list
    (rlk--math-point "Start direction point" :x -1 :y 1)
    (rlk--math-point "End direction point" :x -1 :y 0)
    (rlk--math-point "Row direction" :x 0 :y -1)
    (rlk--math-point "Column direction" :x -1 :y 0))
   (list
    (rlk--math-point "Start direction point" :x -1 :y -1)
    (rlk--math-point "End direction point" :x -1 :y 0)
    (rlk--math-point "Row direction" :x 0 :y 1)
    (rlk--math-point "Column direction" :x -1 :y 0)))
  "Field of view octans.")

(defun rlk--fov-compute-fov (level x y radius)
  "Compute field of view for LEVEL starting from X, Y for the given RADIUS."
  (let ((origin (rlk--math-point "Origin" :x x :y y)))
    (dolist (octan rlk--fov-octans)
      (let ((start-direction (car octan))
            (end-direction (cadr octan))
            (row-direction (nth 2 octan))
            (column-direction (nth 3 octan)))
        (rlk--fov-compute-fov-part level
                                   (rlk--fov-coordinate-transformer "Transformer"
                                                                    :row-direction row-direction
                                                                    :column-direction column-direction)
                                   (rlk--math-line "Start line"
                                                   :points (list origin
                                                                 (add origin start-direction)))
                                   (rlk--math-line "End line"
                                                   :points (list origin
                                                                 (add origin end-direction)))
                                   radius
                                   0)))))

;;;;;;;;;;;;;
;; Applier ;;
;;;;;;;;;;;;;

(defun rlk--fov-apply (level hero)
  "Compute lighting for LEVEL with HERO as origin."
  (dolist (cell-list (get-cells level))
    (dolist (cell cell-list)
      (set-lit cell nil)))
  (rlk--fov-compute-fov level (get-x hero) (get-y hero) 15))

(provide 'roguel-ike/fov)

;;; fov.el ends here
