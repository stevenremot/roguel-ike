;;; hero.el --- Definition of links between hero-data and entity

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
(require 'roguel-ike/hero-data)
(require 'roguel-ike/behaviour/manual)

(defun rlk--entity-create-from-hero-data (hero-data)
  "Create an entity controlled by the user using HERO-DATA."
  (rlk--entity-create (get-race hero-data)
                      (get-stats hero-data)
                      (rlk--behaviour-manual "Manual behaviour")))

(defun rlk--entity-create-hero-data (name entity)
  "Create hero data for name NAME from ENTITY."
  (let ((stats-list '())
        (experience-list '()))
    (dolist (slot-name (get-slot-names (get-stats entity)))
      (let ((stat-slot (get-stat-slot entity slot-name)))
        (setq stats-list
              (append stats-list
                      (list
                       slot-name
                       (cons (get-max-value stat-slot)
                             (get-experience stat-slot)))))))
    (rlk--hero-data "Hero data"
                    :name name
                    :race (get-type (get-race entity))
                    :stats stats-list)))

(provide 'roguel-ike/entity/factory/hero)

;;; hero.el ends here
