;;; level.el --- Contains the data structures relative to levels

;;; Commentary:

;; Defines roguel-ike level structure

;;; Code:
(require 'eieio)
(require 'roguel-ike-entity)

(defclass rlk--level-cell ()
  ((type :initarg :type
         :reader get-type
         :writer set-type
         :type symbol
         :protection :protected
         :documentation "The intrinsic type of the cell
e.g. wall, ground, etc...")
   (lit :initform nil
        :type boolean
        :reader is-lit-p
        :writer set-lit
        :protection private
        :documentation "Tells wether the cell is lit or not."))
  "A class representing a level's cell")

(defmethod is-accessible ((cell rlk--level-cell))
  "Returns t if the cell can be the destination of an entity, nil otherwise."
  nil)

(defmethod has-entity-p ((cell rlk--level-cell))
     "Returns t if an entity stands on the cell, nil otherwise."
     nil)

(defclass rlk--level-cell-ground (rlk--level-cell)
  ((type :initform :ground
         :protection :protected)
   (entity :initform nil
           :reader get-entity
           :writer set-entity
           :type (or rlk--entity boolean)
           :protection :private
           :documentation "The game entity currently on the cell."))
  "A ground cell")

(defmethod has-entity-p ((cell rlk--level-cell-ground))
  "Return `t' if the cell contains an entity, nil otherwise"
  (rlk--entity-child-p (get-entity cell)))

(defmethod is-accessible ((cell rlk--level-cell-ground))
  "Return t if cell is empty, nil otherwise."
  (not (has-entity-p cell)))

(defclass rlk--level-grid ()
  ((cells :initarg :cells
          :type list
          :reader get-cells
          :protection :private
          :documentation "A two-dimensional list of cells"))
  "A two-dimensional grid of cells")

(defmethod width ((grid rlk--level-grid))
  "Return the horizontal number of cells."
  (length (oref grid cells)))

(defmethod height ((grid rlk--level-grid))
  "Return the vertical number of cells."
  (if (eq (width grid) 0)
      0
    (length (car (oref grid cells)))))

(defmethod get-cell-at ((grid rlk--level-grid) x y)
  "Return the cell at position x, y."
  (nth x (nth y (get-cells grid))))

(provide 'roguel-ike-level)
;;; roguel-ike-level.el ends here
