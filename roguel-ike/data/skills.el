;;; skills.el --- Skills definition

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
;; All the skills definition can be found here.

;;; Code:

(require 'roguel-ike/skill)
(require 'roguel-ike/entity)
(require 'roguel-ike/skill/object/fireball)
(require 'roguel-ike/stats/effect)

(rlk--defskill :punch
               "Punch"
               '(:directional :physical)
               '((:strength . 5))
               '((:stamina . 2))
               (lambda (entity dx dy)
                 (let* ((cell (get-neighbour-cell entity dx dy))
                        (target (get-entity cell))
                        (damages nil))
                   (if target
                       (progn
                         (setq damages (compute-damages target
                                                        (round (* (get-base-damages entity)
                                                                  1.2))))
                         (display-message entity
                                          '(Me ("punch" . "punches") "%s for %i damages")
                                          (downcase (get-name target))
                                          damages)
                         (hurt target damages)
                         ;; When the entity has at least 10 strength,
                         ;; Project the enemy
                         (when (>= (get-strength entity) 10)
                           (project target
                                    (cons dx dy)
                                    (- (get-strength entity) 5)))
                         t)
                     (progn
                       (display-message entity "There is no enemy here...")
                       nil)))))

(rlk--defskill :fireball
               "Fireball"
               '(:directional :magical :long-range)
               '((:spirit . 5))
               '((:stamina . 2))
               (lambda (entity dx dy)
                 (let ((fireball (rlk--skill-object-fireball "Fireball"
                                                             :caster entity))
                       (level (get-level entity)))
                   (set-level fireball level)
                   (set-pos fireball (get-x entity) (get-y entity))
                   (add-motion level fireball (cons dx dy) nil)
                   t)))

(rlk--defskill :bite
               "Bite"
               '(:directional :physical)
               '((:strength . 3))
               '((:stamina . 1))
               (lambda (entity dx dy)
                 (let ((cell (get-neighbour-cell entity dx dy))
                       (target nil)
                       (damages 0))
                   (if (and (is-container-p cell) (has-entity-p cell))
                       (progn
                         (setq target (get-entity cell)
                               damages (compute-damages target
                                                        (get-base-damages entity)))
                         (display-message entity
                                          '(Me ("bite" . "bites") "%s for %i damages")
                                          (downcase (get-name target))
                                          damages)
                         (hurt target damages)
                         (apply-on (rlk--effect-get-effect :poison) target)
                         t)
                     (display-message entity "There is no enemy here...")
                     nil))))

(rlk--defskill :war-cry
               "War cry"
               '(:physical)
               '((:strength . 5))
               '((:stamina . 2))
               (lambda (entity)
                 (display-message entity '(Me ("are" . "is") "shouting!"))
                 (dolist (dx '(-1 0 1))
                   (dolist (dy '(-1 0 1))
                     (unless (and (= dx 0)
                                  (= dy 0))
                       (let ((cell (get-neighbour-cell entity dx dy)))
                         (when (and (is-container-p cell)
                                    (has-entity-p cell))
                           (add-motion (get-level entity)
                                       (get-entity cell)
                                       (cons dx dy)
                                       (get-strength entity)))))))
                 (apply-on (rlk--effect-get-effect :tough) entity)))

(rlk--defskill :slash
               "Slash"
               '(:directional :physical)
               '((:strength . 15)
                 (:speed . 15))
               '((:stamina . 5))
               (lambda (entity dx dy)
                 (let ((directions (cond ((= dx 0)
                                     `((0 . ,dy)
                                       (1 . ,dy)
                                       (-1 . ,dy)))
                                    ((= dy 0)
                                     `((,dx . 0)
                                       (,dx . 1)
                                       (,dx . -1)))
                                    (t
                                     `((,dx . ,dy)
                                       (0 . ,dy)
                                       (,dx . 0)))))
                       (cell nil)
                       (target-entity nil)
                       (base-damages (round (* (get-base-damages entity) 1.5)))
                       (damages 0)
                       (slashed nil))
                   (dolist (direction directions)
                     (setq cell (get-neighbour-cell entity (car direction) (cdr direction)))

                     (when (and (is-container-p cell) (has-entity-p cell))
                       (setq target-entity (get-entity cell)
                             damages (compute-damages target-entity base-damages)
                             slashed t)
                       (display-message entity '(Me ("slash" . "slashes") "%s for %i damages.")
                                        (get-name target-entity)
                                        damages)
                       (hurt target-entity damages)

                       (when (>= (get-strength entity) 25)
                         (add-motion (get-level entity) target-entity direction (get-strength entity)))))

                   slashed)))

(provide 'roguel-ike/data/skills)

;;; skills.el ends here
