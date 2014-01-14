;;; entity.el --- Manages game entities

;;; Commentary:

;; Define roguel-ike moving entities

;;; Code:

(require 'eieio)
; (require 'roguel-ike-level)

;; TODO Add a type to grid
(defclass rlk--entity ()
  ((type :initarg :type
         :type symbol
         :reader get-type
         :writer set-type
         :protection :protected
         :documentation "The type of the entity.")
   (x :initform -1
      :type integer
      :reader get-x
      :protection :private
      :documentation "The horizontal position of the entity in the grid.")
   (y :initform -1
      :type integer
      :reader get-y
      :protection :private
      :documentation "The vertical position of the entity in the grid.")
   (grid :reader get-grid
         :writer set-grid
         :protection :private
         :documentation "The grid which contains the entity."))
  "The base class for game entities."
  :abstract t)

(defmethod get-cell ((entity rlk--entity))
  "Return the cell on which stands the entity."
  (get-cell-at (get-grid entity)
               (get-x entity)
               (get-y entity)))


(defmethod set-cell ((entity rlk--entity) cell)
  "Set the new cell of the entity.
This method is instead for private use ONLY.
If you want to change entity position, use set-pos instead."
  (let ((old-cell (get-cell entity)))
    (when (rlk--level-cell-ground-child-p old-cell)
      (set-entity old-cell nil))
    (set-entity cell entity)))


(defmethod set-pos ((entity rlk--entity) x y)
  "Set the new cell pos."
  (let ((cell (get-cell-at (get-grid entity) x y)))
    (when cell
      (set-cell entity cell)
      (oset entity x x)
      (oset entity y y))))

(defmethod try-move ((entity rlk--entity) dx dy)
  "If the entity can move to the cell (x + DX, y + DY), will move to it.
Return t if the entity could move, nil otherwise."
  (let* ((x (+ (get-x entity) dx))
        (y (+ (get-y entity) dy))
        (cell (get-cell-at (get-grid entity) x y)))
    (if (and cell (is-accessible cell))
        (prog2
            (set-pos entity x y)
            t)
      nil)))

(defclass rlk--entity-hero (rlk--entity)
  ((type :initform :hero
         :protection :protected))
  "The main character in the game.")

;;;;;;;;;;;;;
;; Enemies ;;
;;;;;;;;;;;;;

(defclass rlk--entity-enemy (rlk--entity)
  ()
  "Base classe for enemies."
  :abstract t)

(defclass rlk--entity-enemy-rat (rlk--entity-enemy)
  ((type :initform :rat
        :protection :protected))
  "Rat is the weakest enemy.")

(provide 'roguel-ike-entity)
;;; roguel-ike-entity.el ends here
