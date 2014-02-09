;;; path-finding.el --- Path finding system

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
;; Implement A* algorithm to get the shortest path from
;; one point to another in a level.

;;; Code:
(require 'roguel-ike/level/cell)
(require 'roguel-ike-lib/path-finding)

(defun rlk--path-finding-get-cost (level from to)
  "Return the cost for walking in LEVEL from FROM to TO.

Assume that FROM and TO are two contiguous points.

Return nil if going from FROM to TO is impossible."
  (if (is-accessible-p (get-cell-at level (car to) (cdr to)))
      1
    nil))

(defun rlk--path-finding-find-path (origin target level)
  "Return the whole path from ORIGIN to TARGET in LEVEL."
  (roguel-ike-path-finding-find-path origin target level
                                     'rlk--path-finding-get-cost))

(provide 'roguel-ike/path-finding)
;;; path-finding.el ends here
