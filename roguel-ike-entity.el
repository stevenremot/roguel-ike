;;; entity.el --- Manages game entities

;;; Commentary:

;;; Code:

(require 'eieio)

(defclass rlk--entity ()
  ((type :initarg :type
         :type symbol
         :accessor get-type
         :write set-type
         :documentation "The type of the entity.")
   (cell :initarg :cell
         :initform nil
         :accessor get-cell
         :protection :private
         :documentation "The cell on which the entity stands."))
  "The base class for game entities."
  :abstract t)

(defmethod set-cell ((entity rlk--entity) cell)
  "Set the new cell of the entity."
  (let ((old-cell (get-cell entity)))
    (when (rlk--level-cell-child-p old-cell)
      (set-entity old-cell nil))
    (set-entity cell entity)
    (oref entity cell cell)))

(defmethod try-move ((entity rlk--entity) dx dy)
  "If the entity can move to the cell (x + DX, y + DY), will move to it.
Return t if the entity could move, nil otherwise."
  (let ((cell (get-neighbour (get-cell entity) dx dy)))
    (if (and cell (is-accessible cell))
        (prog2
            (set-cell entity cell)
            t)
      nil)))

(defclass rlk--entity-hero (rlk--entity)
  ((type :initform :hero))
  "The main character in the game.")

(provide 'roguel-ike-entity)
;;; roguel-ike-entity.el ends here
