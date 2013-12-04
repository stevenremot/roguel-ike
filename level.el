;;; level.el --- Contains the data structures relative to levels

(require 'eieio)

(defclass roguel-ike-level-cell ()
  ((type :initarg :type
         :accessor type
         :writer type=
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


(provide 'roguel-ike/level)

;;; level.el ends here
