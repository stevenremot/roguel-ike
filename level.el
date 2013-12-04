;;; level.el --- Contains the data structures relative to levels

(require 'eieio)

(defclass roguel-ike-level-cell ()
  ((type :initarg :type
         :accessor get-type
         :writer set-type
         :type symbol
         :protection :private
         :documentation "The intrinsic type of the call
e.g. wall, ground, etc..."))
  "A class representing a level's cell")

(defclass roguel-ike-level-grid ()
  ((cells :initarg :cells
          :type list
          :protection :private
          :documentation "A two-dimensional list of cells"))
  "A two-dimensional grid of cells")

(defmethod width ((grid roguel-ike-level-grid))
  "Returns the horizontal number of cells"
  (length (oref grid cells)))

(defmethod height ((grid roguel-ike-level-grid))
  "Returns the vertical number of cells"
  (if (eq (width grid) 0)
      0
    (length (car (oref grid cells)))))

(defmethod cell-at ((grid roguel-ike-level-grid) x y)
  "Returns the cell at position x, y"
  (nth y (nth x (oref grid cells))))

(provide 'roguel-ike/level)
;;; level.el ends here
