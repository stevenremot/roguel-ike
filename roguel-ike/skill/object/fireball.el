;;; fireball.el --- Fireball object

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
;; Define a fireball object

;;; Code:
(require 'roguel-ike/entity)

(defclass rlk--skill-object-fireball (rlk--level-cell-object)
  ((type :initform :fireball
         :protection :protected)
   (caster :initarg :caster
           :type rlk--entity
           :reader get-caster
           :protection :private
           :documentation "The entity that sent the fireball"))
  "A fireball object that harm entities on contact.")

(defmethod get-layer ((self rlk--skill-object-fireball))
  "See `rlk--level-cell-object'."
  2)

(defmethod accept-other-object-p ((self rlk--skill-object-fireball))
  "See `rlk--level-cell-object'."
  t)

(defmethod collide-with-cell ((self rlk--skill-object-fireball) cell direction energy)
  "Handle collision between the fireball and a cell."
  (let* ((caster (get-caster self))
         (entity nil)
         (level (get-level caster))
         (x (get-x self))
         (y (get-y self))
         (damages 0))
    (when (and (is-container-p cell) (has-entity-p cell))
      (setq entity (get-entity cell)
            damages (compute-damages entity
                                     (round (* (get-base-damages caster)
                                               1.5))))
      (display-message entity
                       (format "%s %s hurt by fireball for %s damages"
                               (get-name entity)
                               (get-verb entity "are" "is")
                               damages))
      (hurt entity damages))
    (when (>= (get-spirit caster) 10)
      (display-message caster "The fireball explodes!")
      (dolist (dx '(-1 0 1))
        (dolist (dy '(-1 0 1))
          (unless (and (= dx 0) (= dy 0))
            (let ((cell (get-cell-at level
                                     (+ x dx)
                                     (+ y dy))))
              (when (and (is-container-p cell)
                         (has-entity-p cell))
                (project (get-entity cell)
                         (cons dx dy)
                         (- (get-spirit caster) 5))))))))
    (remove-object (get-cell self) self)))

(provide 'roguel-ike/skill/object/fireball)
;;; fireball.el ends here
