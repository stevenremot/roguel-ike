;;; string.el --- String -> level convertion

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

(defun rlk--level-create-from-string-list (string-list)
  "Create a level from a string specification.

STRING-LIST is a list of string.
Each string represents a row.  Two characters are possible :

- # : represent a wall
- . : represent a ground cell

Any other cell will be considered as void."
  (let ((cells '())
        (cell-line '()))
    (dolist (line string-list)
      (setq cell-line '())
      (dolist (character (split-string line "" t))
        (setq cell-line
              (append cell-line
                      (list
                       (cond ((string-equal character "#")
                              (rlk--level-cell :type :wall))
                             ((string-equal character ".")
                              (rlk--level-cell-ground))
                             (t
                              (rlk--level-cell :type :void)))))))
      (setq cells (append cells (list cell-line))))
    (rlk--level :cells cells)))

(provide 'roguel-ike/level/factory/string)

;;; string.el ends here
