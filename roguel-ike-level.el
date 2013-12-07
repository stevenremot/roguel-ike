;;; level.el --- Contains the data structures relative to levels

;;; Commentary:

;;; Code:
(require 'eieio)
(require 'roguel-ike-entity)

(defclass roguel-ike-level-cell ()
  ((type :initarg :type
         :accessor get-type
         :writer set-type
         :type symbol
         :protection :private
         :documentation "The intrinsic type of the cell
e.g. wall, ground, etc...")
   (entity :initform nil
           :accessor get-entity
           :writer set-entity
           :protection :private
           :documentation "The game entity currently on the cell"))
  "A class representing a level's cell")

(defmethod has-entity-p ((cell roguel-ike-level-cell))
  "Return `t' if the cell contains an entity, nil otherwise"
  (roguel-ike-entity-child-p (get-entity cell)))

(defclass roguel-ike-level-grid ()
  ((cells :initarg :cells
          :type list
          :accessor get-cells
          :protection :private
          :documentation "A two-dimensional list of cells"))
  "A two-dimensional grid of cells")

(defmethod width ((grid roguel-ike-level-grid))
  "Return the horizontal number of cells"
  (length (oref grid cells)))

(defmethod height ((grid roguel-ike-level-grid))
  "Return the vertical number of cells"
  (if (eq (width grid) 0)
      0
    (length (car (oref grid cells)))))

(defmethod cell-at ((grid roguel-ike-level-grid) x y)
  "Return the cell at position x, y"
  (nth x (nth y (get-cells grid))))

(provide 'roguel-ike-level)
;;; roguel-ike-level.el ends here
