;;; entity.el --- Entity information rendering

;;; Commentary:
;;

;;; Code:
(require 'roguel-ike/entity)
(require 'roguel-ike/graphics/widget/stats)

(defclass rlk--graphics-widget-entity ()
  ((entity :initarg :entity
           :type rlk--entity
           :protection :private
           :documentation "Entity to render.")
   (stats-widget :type rlk--graphics-widget-stats
                 :protection :private
                 :documentation "Sub widget to render statistics."))
  "Render entity's information.")

(defmethod initialize-instance :after ((self rlk--graphics-widget-entity) slots)
  "Initialize subwidgets."
  (oset self stats-widget (rlk--graphics-widget-stats "Stats widget"
                                                      :stats (get-stats (oref self entity)))))

(defmethod render ((self rlk--graphics-widget-entity))
  (concat
   (get-name (oref self entity))
   "\n\n"
   (render (oref self stats-widget))))

(provide 'roguel-ike/graphics/widget/entity)

;;; entity.el ends here
