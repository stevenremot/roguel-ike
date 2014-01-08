;;; entity.el --- Manages game entities

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)

(defclass rlk--entity ()
  ((type :initarg :type
         :type symbol
         :accessor get-type
         :write set-type
         :documentation "The type of the entity.")
   (cell :initarg :cell
         :initform nil
         :accessor get-cell
         :type (or rlk--level-cell boolean)
         :protection :private
         :documentation "The cell on which the entity stands."))
  "The base class for game entities"
  :abstract t)

(defmethod set-cell ((entity rlk--entity) cell)
  "Sets the new cell of the entity"
  (let ((old-cell (get-cell entity)))
    (when (rlk--lvel-cell-child-p old-cell)
      (set-entity old-cell nil))
    (set-entity cell entity)
    (oref entity cell cell)))

(defclass rlk--entity-hero (rlk--entity)
  ((type :initform :hero))
  "The main character in the game")

(provide 'roguel-ike-entity)
;;; roguel-ike-entity.el ends here
