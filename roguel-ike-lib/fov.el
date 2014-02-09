;;; fov.el --- Field of view computing

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
;; Defines field of view calculation system.

;; The field of view is currently computed using recursive shadowcasting.
;; See http://www.roguebasin.com/index.php?title=FOV_using_recursive_shadowcasting
;; to get a introduction to the method.

;;; Code:
(require 'roguel-ike-lib/level)
(require 'roguel-ike-lib/cell)
(require 'roguel-ike-lib/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Coordinates transformer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass roguel-ike-fov-coordinate-transformer ()
  ((row-direction :initarg :row-direction
                  :type cons
                  :reader get-row-direction
                  :protection :private
                  :documentation "Direction of the rows.")
   (column-direction :initarg :column-direction
                     :type cons
                     :reader get-column-direction
                     :protection :private
                     :documentation "Direction of the columns."))
  "Utility used in the iterator.
It knows how to transform initial coordinates to go to the next row / next cell in the row.")

(defmethod get-next-cell ((transformer roguel-ike-fov-coordinate-transformer) pos)
  "Return the next cell's position in the row."
  (let ((row-direction (get-row-direction transformer)))
    (cons (+ (car pos) (car row-direction))
          (+ (cdr pos) (cdr row-direction)))))

(defmethod get-cell-from-line ((transformer roguel-ike-fov-coordinate-transformer) line depth)
  "Return the cell intersecting the LINE at the given DEPTH."
  (let* ((column-direction (get-column-direction transformer))
         (x1 (caar line))
         (y1 (cdar line))
         (dx (car column-direction))
         (dy (cdr column-direction)))

    (if (eql dx 0)
        (let* ((y (+ y1 (* dy depth)))
               (x (round (roguel-ike-math-get-x-from-y line y))))
          (cons x y))
      (let* ((x (+ x1 (* dx depth)))
             (y (round (roguel-ike-math-get-y-from-x line x))))
        (cons x y)))))

(defmethod create-start-line ((transformer roguel-ike-fov-coordinate-transformer) origin start-cell)
  "Create a line going from ORIGIN to a START-CELL corner.

This line is intended to be used as a start limit for the shadowcasting algorithm."
  (let ((column-direction (get-column-direction transformer))
        (row-direction (get-row-direction transformer)))
    (cons origin
          (cons (+ (car start-cell) (* 0.5 (+ (car column-direction) (car row-direction))))
                (+ (cdr start-cell) (* 0.5 (+ (cdr column-direction) (cdr row-direction))))))))

(defmethod create-end-line ((transformer roguel-ike-fov-coordinate-transformer) origin end-cell)
  "Create a line going from ORIGIN to an END-CELL corner.

This line is intended to be used as an end limit for the shadowcasting algorithm."
  (let ((column-direction (get-column-direction transformer))
        (row-direction (get-row-direction transformer)))
    (cons origin
          (cons (- (car end-cell) (* 0.5 (+ (car column-direction) (car row-direction))))
                (- (cdr end-cell) (* 0.5 (+ (cdr column-direction) (cdr row-direction))))))))

(defmethod end-reached-p ((self roguel-ike-fov-coordinate-transformer) origin current-cell-pos last-cell-pos)
  "Return t is the end of the row is reached, nil otherwise."
  (let ((row-direction (get-row-direction self))
        (current-direction (cons (- (car current-cell-pos) (car origin))
                                 (- (cdr current-cell-pos) (cdr origin))))
        (last-direction (cons (- (car last-cell-pos) (car origin))
                              (- (cdr last-cell-pos) (cdr origin)))))
    (> (roguel-ike-math-apply-scalar current-direction row-direction)
       (roguel-ike-math-apply-scalar last-direction row-direction))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shadow casting algorithm ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun roguel-ike-fov-compute-fov-part  (level transformer start-line end-line radius depth)
  "Compute a part of fov for LEVEL.

Navigate in cells using TRANSFORMER.
Compute fov between START-LINE and END-LINE, limited to the given RADIUS,
starting to the given DEPTH."
  (when (<= depth radius)
    (let ((cell-pos (get-cell-from-line transformer start-line depth))
          (last-cell-pos (get-cell-from-line transformer end-line depth))
          (origin (cons (caar start-line) (cdar start-line)))
          (in-wall t)
          (starting-wall t)
          (end-reached nil))

      ;; Firstly check if the first position is not after the last position
      ;; with the powerful method end-reached-p.  As this method is expensive
      ;; prefer the use of equal-p in the loop now that we are sure cell-pos
      ;; will eventually equal last-cell-pos.
      (unless (end-reached-p transformer origin cell-pos last-cell-pos)
        (while (not end-reached)
          (when (<= (roguel-ike-math-get-distance cell-pos origin) radius)
            (let ((cell (get-cell-at level (car cell-pos) (cdr cell-pos))))
              (when cell
                (set-lit cell t)
                (set-visited cell t)

                (if (block-light-p cell)
                    (progn
                      (unless in-wall
                        (setq in-wall t)
                        (unless starting-wall
                          (roguel-ike-fov-compute-fov-part level
                                                           transformer
                                                           start-line
                                                           (create-end-line transformer origin cell-pos)
                                                           radius
                                                           (1+ depth))))
                      (setq start-line (create-start-line transformer origin cell-pos)))
                  (progn
                    (setq in-wall nil
                          starting-wall nil))))))

          (setq end-reached (equal cell-pos last-cell-pos)
                cell-pos (get-next-cell transformer cell-pos))))

      (unless in-wall
        (roguel-ike-fov-compute-fov-part level transformer start-line end-line radius (1+ depth))))))

(defconst roguel-ike-fov-octans
  (list
   (list
    (cons -1 -1) ;; Start direction vector
    (cons 0 -1) ;; End direction vector
    (cons 1 0) ;; Row direction
    (cons 0 -1)) ;; Column direction
   (list
    (cons 1 -1)
    (cons 0 -1)
    (cons -1 0)
    (cons 0 -1))
   (list
    (cons 1 -1)
    (cons 1 0)
    (cons 0 1)
    (cons 1 0))
   (list
    (cons 1 1)
    (cons 1 0)
    (cons 0 -1)
    (cons 1 0))
   (list
    (cons 1 1)
    (cons 0 1)
    (cons -1 0)
    (cons 0 1))
   (list
    (cons -1 1)
    (cons 0 1)
    (cons 1 0)
    (cons 0 1))
   (list
    (cons -1 1)
    (cons -1 0)
    (cons 0 -1)
    (cons -1 0))
   (list
    (cons -1 -1)
    (cons -1 0)
    (cons 0 1)
    (cons -1 0)))
  "Field of view octans.")

(defun roguel-ike-fov-compute-fov (level x y radius)
  "Compute field of view for LEVEL starting from X, Y for the given RADIUS."
  (let ((origin (cons x y)))
    (dolist (octan roguel-ike-fov-octans)
      (let ((start-direction (car octan))
            (end-direction (cadr octan))
            (row-direction (nth 2 octan))
            (column-direction (nth 3 octan)))
        (roguel-ike-fov-compute-fov-part level
                                         (roguel-ike-fov-coordinate-transformer "Transformer"
                                                                                :row-direction row-direction
                                                                                :column-direction column-direction)
                                         (cons origin
                                               (cons (+ (car origin) (car start-direction))
                                                     (+ (cdr origin) (cdr start-direction))))
                                         (cons origin
                                               (cons (+ (car origin) (car end-direction))
                                                     (+ (cdr origin) (cdr end-direction))))
                                         radius
                                         0)))))

(provide 'roguel-ike-lib/fov)
;;; fov.el ends here
