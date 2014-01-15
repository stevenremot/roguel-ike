;;; level.el --- Contains the data structures relative to levels

;;; Commentary:

;; Defines roguel-ike level structure

;;; Code:
(require 'eieio)

;;;;;;;;;;;;
;; Object ;;
;;;;;;;;;;;;

(defclass rlk--level-cell-object ()
  ((type :reader get-type
         :type symbol
         :protection :protected
         :documentation "The intrisic type of the object.")
   (layer :reader get-layer
          :type integer
          :protection :protected
          :documentation "The objects with higher layer values will be drown over the others."))
  "Represent an object lying on a cell."
  :abstract t)

(defmethod is-entity-p ((object rlk--level-cell-object))
  "Return t if the object is an entity, nil otherwise."
  nil)

(defmethod accept-other-object-p ((object rlk--level-cell-object))
  "Return t if another object can stand on the cell, nil otherwise."
  (error "Method accept-other-object-p must be overriden"))

;;;;;;;;;;
;; Cell ;;
;;;;;;;;;;

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
   (objects :initform ()
            :reader get-objects
            :writer set-objects
            :type list
            :protection :private
            :documentation "All the objects lying on the cell."))
  "A ground cell")

(defmethod add-object ((cell rlk--level-cell-ground) object)
  "Add OBJECT into CELL's objects if it is not already in."
  (let ((objects (get-objects cell)))
    (set-objects cell (add-to-list 'objects object))))

(defmethod remove-object ((cell rlk--level-cell-ground) object)
  "Remove OBJECT from CELL's objects if it is in."
  (set-objects cell (delete object (get-objects cell))))

(defmethod get-entity ((cell rlk--level-cell-ground))
  "Return the entity on the CELL if there is currently one.
Return nil otherwise."
  (catch 'entity
    (dolist (object (get-objects cell))
      (when (is-entity-p object)
        (throw 'entity object)))
    (throw 'entity nil)))

(defmethod set-entity ((cell rlk--level-cell-ground) entity)
  "Set the ENTITY standing on the CELL.
If there is already an entity on it, it will be removed."
  (let ((old-entity (get-entity cell)))
    (when old-entity
      (remove-object cell old-entity))
    (when entity
      (add-object cell entity))))

(defmethod has-entity-p ((cell rlk--level-cell-ground))
  "Return `t' if the cell contains an entity, nil otherwise"
  (rlk--level-cell-object-child-p (get-entity cell)))

(defmethod is-accessible ((cell rlk--level-cell-ground))
  "Return t if cell can accept a new object, nil otherwise."
  (catch 'accessible
    (dolist (object (get-objects cell))
      (unless (accept-other-object-p object)
        (throw 'accessible nil)))
    (throw 'accessible t)))

;;;;;;;;;;
;; Grid ;;
;;;;;;;;;;

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
