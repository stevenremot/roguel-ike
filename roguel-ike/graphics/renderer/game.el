;;; game.el --- Game renderer

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
;; Define the game renderer.

;;; Code:
(require 'roguel-ike/level)
(require 'roguel-ike/entity)
(require 'roguel-ike/graphics/faces)

(defclass rlk--graphics-renderer-game ()
  ((symbols-table :initarg :symbols-table
                  :initform ((:ground . ("." . rlk-face-ground))
                             (:wall  . ("#" . rlk-face-wall))
                             (:door-opened . ("~" . rlk-face-door))
                             (:door-closed . ("+" . rlk-face-door))
                             (:void . (" " . rlk-face-default))
                             (:hero . ("@" . rlk-face-hero))
                             (:human . ("@" . rlk-face-human))
                             (:rat . ("r" . rlk-face-rat))
                             (:troll . ("T" . rlk-face-troll)))
                  :reader get-symbols-table
                  :type (or list symbol)
                  :protection :private
                  :documentation "The mapping between object type and ASCII symbol.

See rlk--graphics-ascii-symbol-table for the format.")
   (buffer :initarg :buffer
           :type buffer
           :reader get-target-buffer
           :protection :private
           :documentation "The buffer on which the level will be rendered."))
  "Renderer for game level")

(defmethod draw-cell ((renderer rlk--graphics-renderer-game) cell)
  "Draw the cell on the current buffer, at the current position.
symbols is a hash table whose keys are cell types, and values are
corresponding symbols."
  (let* ((symbol (if (is-lit-p cell)
                     (if (and (is-container-p cell)
                              (get-highest-layer-object cell))
                         (get-type (get-highest-layer-object cell))
                       (get-type cell))
                   (if (is-visited-p cell)
                       (get-type cell)
                     :void)))
         (symbols (get-symbols-table renderer))
         (parameters (cdr (assoc symbol symbols)))
         (character (car parameters))
         (face (if (is-lit-p cell)
                   (cdr parameters)
                 'rlk-face-shadow)))
    (insert (propertize character 'face face))))

(defmethod draw-level ((renderer rlk--graphics-renderer-game) level)
  "Draw the level on the current buffer.
Symbols is the hash table with cell types as key and characters
as values."
  (with-current-buffer (get-target-buffer renderer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (dolist (line (get-cells level))
      (dolist (cell line)
        (draw-cell renderer cell))
      (insert "\n"))
    (setq buffer-read-only t)))

(provide 'roguel-ike/graphics/renderer/game)

;;; game.el ends here
