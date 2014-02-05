;;; arena.el --- Generation of entities for arena

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
(require 'roguel-ike/entity)
(require 'roguel-ike/behaviour/ai)

(defun rlk--level-populator-arena-sum-stats (stats)
  "Sum all the slots of STATS."
  (let ((sum 0))
    (dolist (slot-name (get-slot-names stats))
      (setq sum (+ sum (get-max-value (get-slot stats slot-name)))))
    sum))

(defun rlk--level-populator-arena-sum-plist (stats-list)
  "Sum all the slots in STATS-LIST.

STATS-LIST is a prperty list the represents statistics."
  (let ((sum 0))
    (while stats-list
      (setq sum (+ sum (cadr stats-list)))
      (setq stats-list (cddr stats-list)))
    sum))

(defun rlk--level-populator-arena-get-accessible-cells (level)
  "Return the accessible cells in the LEVEL."
  (let ((cells '()))
    (dotimes (x (width level))
      (dotimes (y (height level))
        (when (is-accessible-p (get-cell-at level x y))
          (add-to-list 'cells (cons x y)))))
    cells))

(defun rlk--level-populator-arena-restrict-races (races max-stats)
  "Return all the races in RACES which base-stats is under MAX-STATS."
  (let ((valid-races '()))
    (dolist (race races)
      (when (<= (rlk--level-populator-arena-sum-plist (get-base-stats race))
                max-stats)
        (add-to-list 'valid-races race)))
    valid-races))

(defun rlk--level-populator-arena-create-entity (available-races
                                                 min-stats
                                                 max-stats)
  "Create a new entity.

Its race is choosen among AVAILABLE-RACES.

MIN-STATS is the minimal global statistics the entity must have.

MAX-STATS is the maximal global statistics the entity can have.

All AVAILABLE-RACES's base stats must be under MAX-STATS."
  (let* ((race (nth (random (length available-races)) available-races))
         (base-stats (get-base-stats race))
         (evolution-stats (get-stats-evolution race))
         (base-stats-sum (rlk--level-populator-arena-sum-plist base-stats))
         (evolution-stats-sum (rlk--level-populator-arena-sum-plist evolution-stats))
         (minimal-level (max 0 (ceiling (- min-stats base-stats-sum)
                                        evolution-stats-sum)))
         (maximal-level (max 0 (ceiling (- max-stats base-stats-sum)
                                        evolution-stats-sum)))
         (choosen-level (+ minimal-level (random (- maximal-level
                                                    minimal-level))))
         (stats '())
         (entity '()))
    (while base-stats
      (setq stats (append stats (list (car base-stats)
                                      (+ (cadr base-stats)
                                         (* choosen-level
                                            (cadr evolution-stats)))))
            base-stats (cddr base-stats)
            evolution-stats (cddr evolution-stats)))

    (rlk--entity-create race
                        stats
                        (rlk--behaviour-ai "AI Behaviour"))))

(defun rlk--level-populator-arena-populate-level (level global-stats max-entities-number)
  "Populate LEVEL with entities.

GLOBAL-STATS is the goal for the sum of the entities' statistics.

MAX-ENTITIES-NUMBER is the maximal number of entities the method can create.

The population mechanism stops when it is reached."
  (let* ((available-cells (rlk--level-populator-arena-get-accessible-cells level))
         (min-stats (ceiling global-stats max-entities-number))
         (max-stats global-stats)
         (available-races (rlk--level-populator-arena-restrict-races rlk--races
                                                                     global-stats))
         (entities '()))
    (while (and (> max-stats 0)
                available-races)
      (let ((entity (rlk--level-populator-arena-create-entity available-races
                                                              min-stats
                                                              max-stats))
            (choosen-cell (nth (random (length available-cells))
                               available-cells)))
        (set-level entity level)
        (set-pos entity (car choosen-cell) (cdr choosen-cell))
        (add-to-list 'entities entity)

        (setq available-cells (delete choosen-cell available-cells)
              max-stats (- max-stats
                           (rlk--level-populator-arena-sum-stats (get-stats entity)))
              available-races (rlk--level-populator-arena-restrict-races available-races max-stats))))
    entities))

(provide 'roguel-ike/level/populator/arena)

;;; arena.el ends here
