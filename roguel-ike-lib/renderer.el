;;; renderer.el --- ASCII level rendering

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
;; Defines a level rendering system.
;; It can generate a colored string representation of a level
;; using a basic configuration.

;;; Code:
(require 'cl-generic)
(require 'roguel-ike-lib/level)
(require 'roguel-ike-lib/cell)

(defclass roguel-ike-renderer ()
  ((symbols-table :initarg :symbols-table
                  :reader get-symbols-table
                  :type list
                  :protection :private
                  :documentation "The mapping between object type and ASCII symbol.

Each key is a game type. This value is specific to a game. The simplest
game type is symbol, like :wall or :hero.

For each key, its value is a cons whcih car is the character to display for this
type (as a string), and cdr is the face to use.

Example:
  '((:ground . (\".\" . face-ground))
    (:wall . (\"#\" . face-wall))
    (:hero . (\"@\" . face-hero)))")
   (unlit-face :initarg :unlit-face
               :initform nil
               :type symbol
               :allow-nil-initform t
               :reader get-unlit-face
               :protection :private
               :documentation "The face to use to display an unlit cell.

If nil, the renderer won't take light in account for rendering.
If non-nil, and if applying `is-lit-p' to a cell return a non-nil value,
this face will be used.

Note that the rendered level's cells must implement `is-lit-p' if unlit-face
is non-nil."))
  "Write a string representation of a level on a buffer.

In order to be displayed, a cell must implement the generic `get-visible-type'.")

(cl-defmethod render-cell ((self roguel-ike-renderer) cell)
  "Insert a colored string representation of CELL in the buffer.

The character is inserted at the current point in the buffer. The
point is then moved forward.

CELL must implement `get-visible-type'."
  (let* ((type (get-visible-type cell))
         (symbols (get-symbols-table self))
         (parameters (cdr (assoc type symbols)))
         (character (car parameters))
         (unlit-face (get-unlit-face self))
         (face (if (and unlit-face (not (is-lit-p cell)))
                   unlit-face
                 (cdr parameters))))
    (insert (propertize character 'face face))))

(cl-defmethod render-level ((self roguel-ike-renderer) level &optional offset size)
  "Insert a colored string representation of LEVEL in the buffer.

OFFSET is the upper left point (as a cons) at which the rendering begins.

SIZE is a cons which tells how many cells are drawn horizontally and vertically.

If OFFSET and SIZE are null, it is set to '(0 . 0) and the level's width and
height.

The text is inserted at the current point in the current buffer.
The point is located at the end of the level representation at the
end of the method.

LEVEL's cells must implement `get-visible-type'."
  (when (null offset)
    (setq offset '(0 . 0)))

  (when (null size)
    (setq size (cons (get-width level) (get-height level))))

  (dotimes (y (cdr size))
    (dotimes (x (car size))
      (render-cell self (get-cell-at level
                                     (+ (car offset) x)
                                     (+ (cdr offset) y))))
    (insert "\n")))



(provide 'roguel-ike-lib/renderer)

;;; renderer.el ends here
