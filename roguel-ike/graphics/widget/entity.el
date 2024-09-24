;;; entity.el --- Entity information rendering

;;; Commentary:
;;

;;; Code:
(require 'cl-generic)
(require 'roguel-ike/entity)
(require 'roguel-ike/stats/effect)
(require 'roguel-ike/graphics/widget/stats)

(defclass rlk--graphics-widget-entity ()
  ((entity :initarg :entity
           :type rlk--entity
           :protection :private
           :documentation "Entity to render.")
   (parts :initarg :parts
          :initform (:name :effects :stats :skills)
          :type list
          :protection :private
          :documentation "The parts of the entity to show.

There is currently:
- :name
- :stats
- :skills
- :effects")
   (stats-widget :type rlk--graphics-widget-stats
                 :protection :private
                 :documentation "Sub widget to render statistics."))
  "Render entity's information.")

(cl-defmethod initialize-instance :after ((self rlk--graphics-widget-entity) slots)
  "Initialize subwidgets."
  (oset self stats-widget (rlk--graphics-widget-stats
                           :stats (get-stats (oref self entity)))))

(cl-defmethod render ((self rlk--graphics-widget-entity))
  (let ((parts (oref self parts))
        (entity (oref self entity))
        (result ""))
    (when (member :name parts)
      (setq result (concat result
                           (get-name entity)
                           "\n\n")))

    (when (and (member :effects parts)
               (get-current-effects entity))
      (setq result (concat result
                           "["
                           (mapconcat (lambda (applier)
                                        (get-name (get-effect applier)))
                                      (get-current-effects entity)
                                      " ")
                           "]\n\n")))

    (when (member :stats parts)
      (setq result (concat result
                           (render (oref self stats-widget))
                           "\n\n")))

    (when (member :skills parts)
      (setq result (apply 'concat
                          result
                          "Skills:\n\n"
                          (mapcar (lambda (skill)
                                    (concat " * "
                                            (get-name skill)
                                            "\n"))
                                   (get-usable-skills entity)))))
    result))

(provide 'roguel-ike/graphics/widget/entity)

;;; entity.el ends here
