;;; stats.el --- Statistics display

;;; Commentary:
;;

;;; Code:
(require 'cl-generic)
(require 'roguel-ike/stats)
(require 'roguel-ike/graphics/faces)


(defclass rlk--graphics-widget-stats ()
  ((stats :initarg :stats
          :type rlk--stats
          :protection :private
          :documentation "Rendered statistics."))
  "Render statistics")

(cl-defmethod get-stat-face ((self rlk--graphics-widget-stats) slot)
  "Return the stat's face according to its levelr elatively to te maximum of the stat."
  (let* ((stat (get-current-value slot))
         (base-stat (get-base-value slot))
         (ratio (/ (float stat) (float base-stat))))
    (cond
     ((> ratio 0.75) 'rlk-face-good-stat)
     ((> ratio 0.25) 'rlk-face-average-stat)
     (t 'rlk-face-bad-stat))))

(cl-defmethod render-stat-slot ((self rlk--graphics-widget-stats) name slot)
  "Render the statistic."
  (concat
   (propertize (concat name " : ") 'face 'rlk-face-default)
   (propertize (format "%d/%d" (get-current-value slot) (get-base-value slot))
                 'face (get-stat-face self slot))
   "\n"))

(cl-defmethod render ((self rlk--graphics-widget-stats))
  "Render statistics on the current buffer at the current point."
  (let
      ((stats (oref self stats)))
    (concat
     (render-stat-slot self "Health      " (get-slot stats :health))
     (render-stat-slot self "Stamina     " (get-slot stats :stamina))
     "\n"
     (render-stat-slot self "Strength    " (get-slot stats :strength))
     (render-stat-slot self "Constitution" (get-slot stats :constitution))
     (render-stat-slot self "Speed       " (get-slot stats :speed))
     (render-stat-slot self "Spirit      " (get-slot stats :spirit)))))

(provide 'roguel-ike/graphics/widget/stats)

;;; stats.el ends here
