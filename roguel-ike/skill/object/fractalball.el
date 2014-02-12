;;; fractalball.el --- A dividing magic projectile

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
;;

;;; Code:
(require 'roguel-ike/entity)

(defclass rlk--skill-object-fractalball (rlk--level-cell-object)
  ((type :initform :fractalball
         :protection :protected)
   (caster :initarg :caster
           :type rlk--entity
           :reader get-caster
           :protection :private
           :documentation "The entity that sent the fractalball.")
   (power :initarg :power
          :type integer
          :reader get-power
          :protection :private
          :documentation "The remaining number of divisions."))
  "A magic projectile that can be divided at impact.")

(defmethod get-layer ((self rlk--skill-object-fractalball))
  "See `rlk--level-cell-object'."
  2)

(defmethod accept-other-object-p ((self rlk--skill-object-fractalball))
  "See `rlk--level-cell-object'."
  t)

(defmethod collide-with-cell ((self rlk--skill-object-fractalball) cell direction energy)
  "Handle collision between the fractalball and a cell."
  (let* ((caster (get-caster self))
         (entity nil)
         (damages 0)
         (inverse-direction (cons (* -1 (car direction))
                                  (* -1 (cdr direction)))))
    (when (and (is-container-p cell) (has-entity-p cell))
      (setq entity (get-entity cell))
      (strike-entity self entity))

    (when (> (random (1+ (get-power self))) 0)
      (let ((sub-power (1- (get-power self))))
        (dolist (dx '(-1 0 1))
          (dolist (dy '(-1 0 1))
            (let ((current-direction (cons dx dy)))
              (unless (member current-direction (list '(0 . 0)
                                                      direction
                                                      inverse-direction))
                (let ((target-cell (get-neighbour-cell self dx dy)))
                  (when (is-container-p target-cell)
                    (if (has-entity-p target-cell)
                        (strike-entity self (get-entity target-cell))
                      (let ((subball (rlk--skill-object-fractalball "Fractal ball"
                                                                    :caster caster
                                                                    :power sub-power)))
                        (set-level subball (get-level self))
                        (set-pos subball (get-x self) (get-y self))
                        (add-motion (get-level self) subball `(,dx . ,dy) nil)))))))))))
    (remove-object (get-cell self) self)))

(defmethod strike-entity ((self rlk--skill-object-fractalball) entity)
  "Hurt ENTITY."
  (let ((caster (get-caster self))
        (damages 0))
    (unless (equal entity caster)
      (setq damages (compute-damages entity
                                     (round (* (get-spirit caster)
                                               1.2))))
      (display-message entity
                       '(Me ("are" . "is") "hurt by fractalball for %i damages.")
                       damages)
      (hurt entity damages))))

(provide 'roguel-ike/skill/object/fractalball)

;;; fractalball.el ends here
