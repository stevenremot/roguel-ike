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

(defclass rlk--behaviour-ai (rlk--behaviour)
  ()
  "Behaviour of entities controlled by the computer.")


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

(defmethod do-action ((self rlk--behaviour-ai) callback)
  "See rlk--behaviour."
  (let ((nb-turns (move-randomly self)))
    (spend-time (get-entity self) nb-turns)
    (funcall callback nb-turns)))

(provide 'roguel-ike/behaviour/ai)

;;; ai.el ends here
