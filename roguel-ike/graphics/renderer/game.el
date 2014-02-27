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
(require 'roguel-ike-lib/renderer)

(defcustom roguel-ike-default-graphics
  '((:ground . ("." . rlk-face-ground))
    (:wall  . ("#" . rlk-face-wall))
    (:door-opened . ("~" . rlk-face-door))
    (:door-closed . ("+" . rlk-face-door))
    (:stairs-up . ("<" . rlk-face-stairs))
    (:stairs-down . (">" . rlk-face-stairs))
    (:void . (" " . rlk-face-default))
    (:hero . ("@" . rlk-face-hero))
    (:human . ("@" . rlk-face-human))
    (:rat . ("r" . rlk-face-rat))
    (:troll . ("T" . rlk-face-troll))
    (:gnome . ("g" . rlk-face-gnome))
    (:fireball . ("o" . rlk-face-fireball))
    (:fractalball . ("o" . rlk-face-fractalball)))
  "The default mapping between game type and graphics."
  :group 'roguel-ike)

(defclass rlk--graphics-renderer-game ()
  ((renderer :initarg :renderer
             :type roguel-ike-renderer
             :reader get-renderer
             :protection :private
             :documentation "The renderer used to generate level's strings.")
   (buffer :initarg :buffer
           :type buffer
           :reader get-target-buffer
           :protection :private
           :documentation "The buffer on which the level will be rendered."))
  "Renderer for game level")

(defmethod initialize-instance :after ((self rlk--graphics-renderer-game) slots)
  "Initialize renderer."
  (unless (slot-boundp self 'renderer)
    (oset self renderer
          (roguel-ike-renderer "Level renderer"
                               :symbols-table roguel-ike-default-graphics
                               :unlit-face 'rlk-face-shadow))))

(defmethod draw-level ((self rlk--graphics-renderer-game) level center)
  "Draw the level on the current buffer.
Symbols is the hash table with cell types as key and characters
as values."
  (with-current-buffer (get-target-buffer self)
    (setq buffer-read-only nil)
    (erase-buffer)
    (render-level (get-renderer self) level center)
    (setq buffer-read-only t)))

(provide 'roguel-ike/graphics/renderer/game)

;;; game.el ends here
