;;; layout.el --- Generate a level from a layout

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
(require 'roguel-ike/level/cell/ground)
(require 'roguel-ike/interactive-object/door)

(defun rlk--level-create-from-layout (layout)
  "Create a level object from LAYOUT.

LAYOUT is a two-dimensional sequence containing symbols representing a cell.
These symbols can be:

- :wall
- :ground
- :door"
  (let ((cells '())
        line
        current-element
        (doors '())
        level
        layout-line)
    (dotimes (y (length layout))
      (setq line nil
            layout-line (elt layout y))
      (dotimes (x (length layout-line))
        (setq current-element (elt layout-line x)
              line (append line
                           (list (cond ((eq current-element :wall) (rlk--level-cell :type :wall))
                                       ((eq current-element :ground) (rlk--level-cell-ground))
                                       ((eq current-element :door)
                                        (let ((cell (rlk--level-cell-ground)))
                                          (add-object cell (rlk--interactive-object-door))
                                          cell)))))))
      (setq cells (append cells (list line))))
    (setq level (rlk--level :cells cells))
    level))

(provide 'roguel-ike/level/factory/layout)

;;; layout.el ends here
