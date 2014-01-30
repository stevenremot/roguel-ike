;;; stats.el --- Statistics renderer

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
;; Define the statistics renderer.

;;; Code:
(require 'roguel-ike/stats)
(require 'roguel-ike/graphics/faces)

(defclass rlk--graphics-renderer-stats ()
  ((stats :initarg :stats
          :type rlk--stats
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
         (ratio (/ (float stat) (float max-stat))))
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

(provide 'roguel-ike/graphics/renderer/stats)

;;; stats.el ends here
