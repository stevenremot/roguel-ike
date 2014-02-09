;;; fov.el --- Field of view system for roguel-ike

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

;;; Code:
(require 'roguel-ike-lib/fov)
(require 'roguel-ike/entity)

(defun rlk--fov-apply (level hero)
  "Compute lighting for LEVEL with HERO as origin."
  (dolist (cell-list (get-cells level))
    (dolist (cell cell-list)
      (set-lit cell nil)))
  (roguel-ike-fov-compute-fov level (get-x hero) (get-y hero) 15))

(provide 'roguel-ike/fov)
;;; fov.el ends here
