;;; ai.el --- Artificial intelligence control

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
;; This behaviour let the computer control the entity.

;;; Code:
(require 'roguel-ike/behaviour)
(require 'roguel-ike-lib/los)
(require 'roguel-ike/path-finding)

(defclass rlk--behaviour-ai (rlk--behaviour)
  ((target :type rlk--entity
           :reader get-target
           :writer set-target
           :protection :private
           :documentation "The entity hunted by the behaviour.")
   (line-of-sight :initarg :line-of-sigth
                  :initform nil
                  :type (or integer null)
                  :reader get-line-of-sight
                  :writer set-line-of-sight
                  :protection :private
                  :documentation "The behaviour won't hunt target over this value.

If it is nil, there is no distance limitation."))
  "Behaviour of entities controlled by the computer.")

(defmethod do-action ((self rlk--behaviour-ai) callback)
  "See rlk--behaviour."
  (let ((nb-turns (try-hunt-target self)))
    (unless (numberp nb-turns)
      (setq nb-turns (move-randomly self)))
    (spend-time (get-entity self) nb-turns)
    nb-turns))

(defmethod try-hunt-target ((self rlk--behaviour-ai))
  "Try to move to the target.

Will attack it if it is nearby."
  (let* ((target-entity (get-target self))
         (entity (get-entity self))
         (level (get-level entity))
         (x1 (get-x entity))
         (y1 (get-y entity))
         (x2 (get-x target-entity))
         (y2 (get-y target-entity))
         (origin (cons x1 y1))
         (target (cons x2 y2))
         (x-offset (- x1 x2))
         (y-offset (- y1 y2))
         (distance (sqrt (+ (* x-offset x-offset)
                            (* y-offset y-offset))))
         (line-of-sight (get-line-of-sight self)))
    (if (or (null line-of-sight)
            (<= distance line-of-sight))
        (if (< distance 2)
            (progn
              (attack entity target-entity)
              1)
          (when (roguel-ike-los-can-see-p origin target level)
            (let ((direction (rlk--path-finding-get-direction-to-target origin
                                                                        target
                                                                        level)))
              (when direction
                  (try-move entity (car direction) (cdr direction))
                  1)))))))

(defmethod move-randomly ((self rlk--behaviour-ai))
  "Try to move on a random neighbour cell.
Return the number of turns spent if it could move, 1 for waiting otherwise."
  (let* ((entity (get-entity self))
         (accessible-cells '())
         (level (get-level entity))
         (choosen-cell nil))
    (dotimes (i 3)
      (dotimes (j 3)
        (let*
            ((dx (- i 1))
             (dy (- j 1))
             (x (+ (get-x entity) (- i 1)))
             (y (+ (get-y entity) (- j 1)))
             (cell (get-cell-at level x y)))
          (when (is-accessible-p cell)
            (add-to-list 'accessible-cells (cons dx dy))))))
    ;; If there are accessible cells, move. Otherwise, wait.
    (when accessible-cells
      (setq choosen-cell (nth (random (length accessible-cells))
                              accessible-cells))
      (try-move entity (car choosen-cell) (cdr choosen-cell)))
    1))


(provide 'roguel-ike/behaviour/ai)

;;; ai.el ends here
