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
(require 'cl-generic)
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
           :documentation "The buffer on which the level will be rendered.")
   (offset :type cons
           :reader get-offset
           :protection :private
           :documentation "The current offset for targeting the center."))
  "Renderer for game level")

(cl-defmethod initialize-instance :after ((self rlk--graphics-renderer-game) slots)
  "Initialize renderer."
  (unless (slot-boundp self 'renderer)
    (oset self renderer
          (roguel-ike-renderer :symbols-table roguel-ike-default-graphics
                               :unlit-face 'rlk-face-shadow))))

(cl-defmethod draw-level ((self rlk--graphics-renderer-game) level center)
  "Draw the level on the current buffer.
Symbols is the hash table with cell types as key and characters
as values."
  (with-current-buffer (get-target-buffer self)
    (let* ((minimum-x 0)
         (minimum-y 0)
         (level-width (get-width level))
         (level-height (get-height level))
         (width level-width)
         (height level-height)
         (window (get-buffer-window (current-buffer)))
         (window-width (window-width window))
         (window-height (1- (window-height window))))
    (when (< window-width width)
      (setq width window-width
            minimum-x (max 0 (min (- (car center) (/ window-width 2))
                                  (- level-width window-width)))))

    (when (< window-height height)
      (setq height window-height
            minimum-y (max 0 (min (- (cdr center) (/ window-height 2))
                                  (- level-height window-height)))))

    (oset self offset (cons minimum-x minimum-y))
    (setq buffer-read-only nil)
    (erase-buffer)
    (render-level (get-renderer self) level (get-offset self)
                  (cons width height))
    (setq buffer-read-only t)
    (goto-char (point-min)))))

(provide 'roguel-ike/graphics/renderer/game)

;;; game.el ends here
