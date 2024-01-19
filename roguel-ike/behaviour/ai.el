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
(require 'cl-generic)
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

If it is nil, there is no distance limitation.")
   (skill-probability :initarg :skill-probability
                      :initform 50
                      :type integer
                      :reader get-skill-probability
                      :protection :private
                      :documentation "The probability the entity will use a skill when it can.")
   (memorized-path :initform nil
                   :type list
                   :protection :private
                   :documentation "The current path the entity is following."))
  "Behaviour of entities controlled by the computer.")

(cl-defmethod do-action ((self rlk--behaviour-ai) callback)
  "See rlk--behaviour."
  (let ((nb-turns (try-hunt-target self)))
    (unless (numberp nb-turns)
      (setq nb-turns (move-randomly self)))
    (spend-time (get-entity self) nb-turns)
    nb-turns))

(cl-defmethod try-hunt-target ((self rlk--behaviour-ai))
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
         (line-of-sight (get-line-of-sight self))
         (attacked nil))

    ;; If it see the entity and it is at contact, attack it.
    ;; If the entity is far away, maybe attack it, and find a path
    ;; to go to it.
    (when (or (null line-of-sight)
            (<= distance line-of-sight))
        (if (< distance 2)
            (progn
              (attack-at-contact self)
              (setq attacked t))
          (when (roguel-ike-los-can-see-p origin target level)
            (setq attacked (try-attack-at-range self))
            (oset self memorized-path (cdr (rlk--path-finding-find-path origin
                                                                        target
                                                                        level))))))

    ;; If no action have been done before, try to move with the memorized
    ;; path if any. If it cannot move to the next path's direction (eg an
    ;; entity is standing on the cell), forget it.
    (if attacked
        1
      (let ((direction (peek-next-direction self)))
        (when direction
          (if (try-move entity (car direction) (cdr direction))
              (progn
                (pop-next-direction self)
                1)
            (if (try-open-door self direction)
                1
              (oset self memorized-path nil))))))))

(cl-defmethod try-open-door ((self rlk--behaviour-ai) direction)
  "Try to open a door in the relative DIRECTION.

Return t if it could, nil otherwise."
  (let* ((entity (get-entity self))
         (cell (get-neighbour-cell entity (car direction) (cdr direction))))
    (catch 'door-opened
      (dolist (object (get-objects cell))
        (when (equal (get-type object) :door-closed)
          (do-action object entity :open)
          (throw 'door-opened t)))
      nil)))

(cl-defmethod get-relative-direction ((self rlk--behaviour-ai) point)
  "Return the direction from ai's entity to POINT."
  (let ((entity (get-entity self)))
    (cons (- (car point) (get-x entity))
          (- (cdr point) (get-y entity)))))

(cl-defmethod peek-next-direction ((self rlk--behaviour-ai))
  "Return the next direction of the followed path if any."
  (let ((entity (get-entity self))
        (point (car (oref self memorized-path))))
    (when point
      (get-relative-direction self point))))

(cl-defmethod pop-next-direction ((self rlk--behaviour-ai))
  "Return the next direction of the followed path if any.

Remove it from the path."
  (let ((entity (get-entity self))
        (point (car (oref self memorized-path))))
    (when point
      (oset self memorized-path (cdr (oref self memorized-path)))
      (get-relative-direction self point))))

(cl-defmethod move-randomly ((self rlk--behaviour-ai))
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

(cl-defmethod get-long-range-skills ((self rlk--behaviour-ai))
  "Return the skills that could be used even when the target is far away."
  (let ((skills (get-usable-skills (get-entity self)))
        (range-skills '()))
    (dolist (skill skills)
      (when (or (has-tag-p skill :long-range)
                (has-tag-p skill :support))
        (push skill range-skills)))
    range-skills))

(cl-defmethod get-contact-skills ((self rlk--behaviour-ai))
  "Return the skills that can be used only at target's contact."
  (let ((skills (get-usable-skills (get-entity self)))
        (contact-skills '()))
    (dolist (skill skills)
      (unless (has-tag-p skill :long-range)
        (push skill contact-skills)))
    contact-skills))

(cl-defmethod attack-at-contact ((self rlk--behaviour-ai))
  "Attack entity, assuming it is at contact."
  (let ((target-entity (get-target self))
        (contact-skills (get-contact-skills self)))
    (unless (and contact-skills
                 (> (random 100) (get-skill-probability self))
                 (use-skill self (nth (random (length contact-skills))
                                      contact-skills)))
      (attack (get-entity self) target-entity))))

(cl-defmethod try-attack-at-range ((self rlk--behaviour-ai))
  "Try to attack the target, assuming it is not at contact.

Return t when it succeeded."
  (let* ((range-skills (get-long-range-skills self))
         (target-entity (get-target self))
         (entity (get-entity self))
         (x-offset (abs (- (get-x entity) (get-x target-entity))))
         (y-offset (abs (- (get-y entity) (get-y target-entity))))
         (skill (nth (random (length range-skills))
                              range-skills)))
    (and range-skills
         (or (not (has-tag-p skill :directional))
             (= x-offset 0)
             (= y-offset 0)
             (= x-offset y-offset))
         (> (random 100) (get-skill-probability self))
         (use-skill self skill))))

(cl-defmethod use-skill ((self rlk--behaviour-ai) skill)
  "Use SKILL.

Assume all checks have been done before.

Return t when the skill's action could be done."
  (let ((arguments '())
        (target-entity (get-target self))
        (entity (get-entity self)))
    (when (has-tag-p skill :directional)
      (let* ((x-offset (- (get-x target-entity) (get-x entity)))
             (y-offset (- (get-y target-entity) (get-y entity)))
             (dx (cond ((> x-offset 0) 1)
                       ((< x-offset 0) -1)
                       (t 0)))
             (dy (cond ((> y-offset 0) 1)
                       ((< y-offset 0) -1)
                       (t 0))))
        (setq arguments (append arguments (list dx dy)))))
    (apply 'use-skill entity skill arguments)))

(provide 'roguel-ike/behaviour/ai)

;;; ai.el ends here
