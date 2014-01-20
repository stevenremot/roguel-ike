;;; graphics.el --- In charge of drawing things

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
;; In charge of roguel-ike rendering

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-entity)

;;;;;;;;;;;;;;;;
;; Game faces ;;
;;;;;;;;;;;;;;;;

(defgroup rlk-faces
  '()
  "Group for rlk faces"
  :group 'emacs)

(defface rlk-face-default
  '()
  "Standard roguel-ike face"
  :group 'rlk-faces)

(defface rlk-face-wall
  '((((class color) (min-colors 88))
      :inherit 'rlk-face-default))
  "Wall face"
  :group 'rlk-faces)

(defface rlk-face-ground
  '((((class color) (min-colors 88))
      :inherit 'rlk-face-default
      :foreground "dim gray"))
  "Ground face"
  :group 'rlk-faces)

(defface rlk-face-door
  '((((class color) (min-colors 88))
     :inherit 'rlk-face-default
     :foreground "sandy brown"))
  "Door face"
  :group 'rlk-faces)

(defface rlk-face-hero
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "yellow3"))
  "Hero face"
  :group 'rlk-faces)

(defface rlk-face-rat
  '((((class color) (min-colors 8))
    :inherit 'rlk-face-default
    :foreground "red"))
  "Rat face"
  :group 'rlk-faces)

(defface rlk-face-good-stat
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "green"))
  "Good statistic face"
  :group 'rlk-faces)

(defface rlk-face-average-stat
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "yellow"))
  "Average statistic face"
  :group 'rlk-faces)

(defface rlk-face-bad-stat
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "red"))
  "Bad statistic face"
  :group 'rlk-faces)

;;;;;;;;;;;;;;;;;;;
;; Game Renderer ;;
;;;;;;;;;;;;;;;;;;;

(defclass rlk--graphics-renderer-game ()
  ((symbols-table :initarg :symbols-table
                  :initform ((:ground . ("." . rlk-face-ground))
                             (:wall  . ("#" . rlk-face-wall))
                             (:door-opened . ("~" . rlk-face-door))
                             (:door-closed . ("+" . rlk-face-door))
                             (:void . (" " . rlk-face-default))
                             (:hero . ("@" . rlk-face-hero))
                             (:rat . ("r" . rlk-face-rat)))
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
         (face (cdr parameters)))
    (insert (propertize character 'face face))))

(defmethod draw-level ((renderer rlk--graphics-renderer-game) level)
  "Draw the level on the current buffer.
Symbols is the hash table with cell types as key and characters
as values."
  (with-current-buffer (get-target-buffer renderer)
    (erase-buffer)
    (dolist (line (get-cells level))
      (dolist (cell line)
        (draw-cell renderer cell))
      (insert "\n"))))

;;;;;;;;;;;;;;;;;;;;
;; Stats renderer ;;
;;;;;;;;;;;;;;;;;;;;

(defclass rlk--graphics-renderer-stats ()
  ((stats :initarg :stats
          :type rlk--entity-stats
          :protection :private
          :documentation "Rendered statistics.")
   (buffer :initarg :buffer
           :type buffer
           :protection :private
           :documentation "Buffer on which statistcis are renderered."))
  "Render statistics")

(defmethod get-stat-face ((self rlk--graphics-renderer-stats) slot)
  "Return the stat's face according to its levelr elatively to te maximum of the stat."
  (let* ((stat (get-current-value slot))
         (max-stat (get-max-value slot))
         (ratio (/ stat max-stat)))
    (cond
     ((> ratio 0.75) 'rlk-face-good-stat)
     ((> ratio 0.25) 'rlk-face-average-stat)
     (t 'rlk-face-bad-stat))))

(defmethod draw-stat-slot ((self rlk--graphics-renderer-stats) name slot)
  "Draw the statistic."
  (insert (propertize (concat name " : ") 'face 'rlk-face-default))
  (insert (propertize (format "%d/%d" (get-current-value slot) (get-max-value slot))
                      'face (get-stat-face self slot)))
  (insert "\n"))

(defmethod draw-stats ((self rlk--graphics-renderer-stats))
  "Draw hero statistics on the buffer"
  (let
      ((stats (oref self stats)))
    (with-current-buffer (oref self buffer)
      (erase-buffer)
      (draw-stat-slot self "Health      " (get-slot stats :health))
      (draw-stat-slot self "Stamina     " (get-slot stats :stamina))
      (insert "\n")
      (draw-stat-slot self "Strength    " (get-slot stats :strength))
      (draw-stat-slot self "Constitution" (get-slot stats :constitution))
      (draw-stat-slot self "Speed       " (get-slot stats :speed))
      (draw-stat-slot self "Spirit      " (get-slot stats :spirit)))))

(provide 'roguel-ike-graphics)
;;; roguel-ike-graphics.el ends here
