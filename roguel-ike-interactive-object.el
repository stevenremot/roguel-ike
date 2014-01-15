;;; roguel-ike-interactive-object.el --- Level's interactive objects

;;; Commentary:

;; Defines interactive objects in the level

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)

;;;;;;;;;;;;;;;;
;; Base class ;;
;;;;;;;;;;;;;;;;

(defclass rlk--interactive-object (rlk--level-cell-object)
  ()
  "Base class for interactive objects."
  :abstract t)

(defmethod get-layer ((object rlk--interactive-object))
  "See rlk--level-cell-object."
  2)

(defmethod do-action ((object rlk--interactive-object) hero action)
  "Do the ACTION when the HERO interacts with it.
Return t when the action was successfull, nil otherwise."
  (error "Method do-action must be overriden"))

;;;;;;;;;;
;; Door ;;
;;;;;;;;;;

(defclass rlk--interactive-object-door (rlk--interactive-object)
  ((opened :initform nil
          :type boolean
          :reader is-opened-p
          :protection :private
          :documentation "Tell whether the door is opened or not."))
  "A door that can be opened and closed.
An entity cannot pass when door is closed.")

(defmethod get-type ((door rlk--interactive-object-door))
  "See rlk--level-cell-object."
  (if (is-opened-p door)
      :door-opened
    :door-closed))

(defmethod accept-other-object-p ((door rlk--interactive-object-door))
  "See rlk--level-cell-object."
  (is-opened-p door))

(defmethod do-action ((door rlk--interactive-object-door) hero action)
  "Close / open the door.
When the action is :open and the door is closed, open it.
When the action is :close and the door is open, close it."
  (cond
   ((and (is-opened-p door) (equal action :close))
    (oset door opened nil)
    t)
   ((and (not (is-opened-p door)) (equal action :open))
    (oset door opened t)
    t)
   (t
    nil)))

(provide 'roguel-ike-interactive-object)

;;; roguel-ike-interactive-object.el ends here
