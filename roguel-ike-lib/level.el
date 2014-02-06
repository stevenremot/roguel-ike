;;; level.el --- Generic level definition

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
;; Defines the basic methods that may be implemented
;; in order to allow a EIEIO class to be considered as
;; a roguelike level.

;; Implementing these generics allow a data structure to
;; be used in other parts of the library.

;; A level is simply considered as a two-dimensional array of cells.
;; The generics bellow define methods to manipulate level cells.

;;; Code:
(require 'eieio)

(defgeneric get-height (level)
  "Return the number of rows of LEVEL.")

(defgeneric get-width (level)
  "Return the number of columns of LEVEL.")

(defgeneric get-cell-at (level x y)
  "Return the cell of the level in the Xth column and the Yth row.

The first column and row are at position 0.")

(provide 'roguel-ike-lib/level)
;;; level.el ends here
