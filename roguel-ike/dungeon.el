;;; dungeon.el --- An infinite sequence of levels

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
(require 'roguel-ike/level)
(require 'roguel-ike/level/factory/all-rooms)
(require 'roguel-ike/interactive-object/stairs)
(require 'roguel-ike/entity)

(defclass rlk--dungeon ()
  ((levels :type list
           :protection :private
           :documentation "The generated levels.")
   (dispatcher :type roguel-ike-dispatcher
               :reader get-dispatcher
               :protection :private
               :documentation "The event dispatcher of the dungeon.

The dispatched events are:
- :changed-level LEVEL when the hero moved to the level LEVEL
- :reached-level LEVEL when the hero discovered the level LEVEL
  for the first time."))
  "An infinite sequence of levels connected by stairs.")

(defmethod initialize-instance :after ((self rlk--dungeon) slots)
  (oset self levels '())
  (oset self dispatcher (roguel-ike-dispatcher "Dungeon dispatcher")))

(defmethod create-level ((self rlk--dungeon) level-number)
  "Create a new level for LEVEL-NUMBER, adding stairs to it."
  (let* ((level (rlk--level-create-all-rooms 50 25 5 20 85))
         (cells (mapcar (lambda (pos)
                          (get-cell-at level (car pos) (cdr pos)))
                        (get-accessible-cells-pos level)))
         start-cell
         end-cell
         populator)
    (setq start-cell (nth (random (length cells)) cells)
          cells (delq start-cell cells)
          end-cell (nth (random (length cells)) cells))
    (add-object start-cell (rlk--interactive-object-stairs "Downstairs"
                                                           :direction :up
                                                           :callback (apply-partially
                                                                      'teleport-to-level
                                                                      self
                                                                      (1- level-number)
                                                                      :down)))
    (add-object end-cell (rlk--interactive-object-stairs "Upstairs"
                                                           :direction :down
                                                           :callback (apply-partially
                                                                      'teleport-to-level
                                                                      self
                                                                      (1+ level-number)
                                                                      :up)))
    level))

(defmethod teleport-to-level ((self rlk--dungeon) level-number from entity)
  "Teleport ENTITY to the level with number LEVEL-NUMBER.

FROM is the origin of the entity. It can be either :up or :down.

If the level does not exist, :level-reached will be dispatched, and a new level will be created."
  (if (< level-number 0)
      (display-message entity '(Me "won't leave so easily."))
    (let ((levels (oref self levels))
          level
          position)
      (when (>= level-number (length levels))
        (oset self levels (append levels (list (create-level self level-number))))
        (setq levels (oref self levels))
        (dispatch (get-dispatcher self) :reached-level level-number))
      (setq level (nth level-number levels)
            position (if (eq from :up)
                          (get-start-position self level)
                        (get-end-position self level)))
      (set-level entity level)
      (set-pos entity (car position) (cdr position))
      (when (is-hero-p entity)
        (dispatch (get-dispatcher self) :changed-level level-number)))))

(defmethod get-start-position ((self rlk--dungeon) level)
  "Return the position of the down stairs in LEVEL."
  (catch 'start-position
    (dotimes (x (get-width level))
      (dotimes (y (get-height level))
        (let ((cell (get-cell-at level x y)))
          (when (is-container-p cell)
            (dolist (object (get-objects cell))
              (when (eq :stairs-up (get-type object))
                (throw 'start-position `(,x . ,y))))))))
    nil))



(defmethod get-end-position ((self rlk--dungeon) level)
  "Return the position of the up stairs in LEVEL."
  (catch 'end-position
    (dotimes (x (get-width level))
      (dotimes (y (get-height level))
        (let ((cell (get-cell-at level x y)))
          (when (is-container-p cell)
            (dolist (object (get-objects cell))
              (when (eq :stairs-down (get-type object))
                (throw 'end-position `(,x . ,y))))))))
    nil))

(defmethod get-level ((self rlk--dungeon) level-number)
  "Return the level at LEVEL-NUMBER."
  (nth level-number (oref self levels)))

(provide 'roguel-ike/dungeon)

;;; dungeon.el ends here
