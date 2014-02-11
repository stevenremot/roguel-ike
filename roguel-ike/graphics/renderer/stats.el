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
(require 'roguel-ike/graphics/widget/entity)

(defclass rlk--graphics-renderer-stats ()
  ((widget :initarg :widget
           :type rlk--graphics-widget-entity
           :protection :private
           :documentation "Rendered statistics.")
   (buffer :initarg :buffer
           :type buffer
           :protection :private
           :documentation "Buffer on which statistcis are renderered."))
  "Render statistics")

(defmethod initialize-instance ((self rlk--graphics-renderer-stats) slots)
  (let ((stats (plist-get slots :stats))
        (buffer (plist-get slots :buffer)))
    (call-next-method self (list :widget (rlk--graphics-widget-entity "Entity widget"
                                                                      :entity (plist-get slots :entity)
                                                                      :parts '(:effects
                                                                               :stats))
                                 :buffer buffer))))

(defmethod draw-stats ((self rlk--graphics-renderer-stats))
  "Draw hero statistics on the buffer"
  (with-current-buffer (oref self buffer)
    (erase-buffer)
    (insert (render (oref self widget)))))

(provide 'roguel-ike/graphics/renderer/stats)

;;; stats.el ends here
