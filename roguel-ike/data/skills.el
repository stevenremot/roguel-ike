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
                                                        (* (get-base-damages entity)
                                                           2)))
                         (display-message entity
                                          (format "%s %s %s for %i damages"
                                                  (get-name entity)
                                                  (get-verb entity "punch" "punches")
                                                  (downcase (get-name target))
                                                  damages))
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
               '(:directional :magical)
               '((:spirit . 5))
               '((:stamina . 2))
               (lambda (entity dx dy)
                 (let ((fireball (rlk--skill-object-fireball "Fireball"
                                                             :caster entity))
                       (level (get-level entity)))
                   (set-level fireball level)
                   (set-pos fireball (get-x entity) (get-y entity))
                   (add-motion level fireball (cons dx dy) nil))))

(provide 'roguel-ike/data/skills)

;;; skills.el ends here
