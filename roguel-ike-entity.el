;;; entity.el --- Manages game entities

;;; Commentary:

;; Define roguel-ike moving entities

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-message)
(require 'roguel-ike-interactive-object)

;;;;;;;;;;;;;;;;;;;;;
;; Abstract entity ;;
;;;;;;;;;;;;;;;;;;;;;

(defclass rlk--entity (rlk--level-cell-object)
  ((max-health :type integer
               :reader get-max-health
               :writer set-max-health
               :protection :protected
               :documentation "The maximum health the entity can have.")
   (current-health :type integer
                   :protection :private
                   :documentation "The health the entity currently has.")
   (time-delay :initform 0
               :type integer
               :reader get-time-delay
               :protection :protected
               :documentation "The amount of time the entity have to wait / spend.")
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
   (grid :type rlk--level-grid
         :reader get-grid
         :writer set-grid
         :protection :private
         :documentation "The grid which contains the entity.")
   (message-logger :type rlk--message-logger
                   :reader get-message-logger
                   :protection :protected
                   :documentation "The logger used to display entitie's messages."))
  "The base class for game entities."
  :abstract t)

(defmethod get-layer ((entity rlk--entity))
  "See rlk--level-cell-object."
  3)

(defmethod is-entity-p ((entity rlk--entity))
  "See rlk--level-cell-object."
  t)

(defmethod accept-other-object-p ((entity rlk--entity))
  "See rlk--level-cell-object."
  nil)

(defmethod display-message ((entity rlk--entity) message)
  "Use the message logger to display a message."
  (display-message (get-message-logger entity) message))

(defmethod add-time-delay ((entity rlk--entity) time)
  "Add TIME to time-delay."
  (oset entity time-delay (- (get-time-delay entity) time)))

(defmethod spend-time-delay ((entity rlk--entity) time)
  "Subtract TIME to time-delay."
  (oset entity time-delay (+ (get-time-delay entity) time)))

(defmethod can-do-action ((entity rlk--entity))
  "Return t is the entity can do an action, nil otherwise."
  (< (get-time-delay entity) 0))

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
    (if (and cell (is-accessible-p cell))
        (prog2
            (set-pos entity x y)
            (spend-time-delay entity 1)
            t))
    nil))

(defmethod entity-alive-p ((entity rlk--entity))
  "Return t if the ENTITY is alive, nil otherwise."
  (> (get-current-health entity) 0))

(defmethod get-current-health ((entity rlk--entity))
  "Return the ENTITY's current health."
  (unless (slot-boundp entity 'current-health)
    (set-current-health entity (get-max-health entity)))
  (oref entity current-health))

(defmethod set-current-health ((entity rlk--entity) health)
  "Set ENTITY's current health to HEALTH."
  (oset entity current-health health))

(defmethod hurt ((entity rlk--entity) points)
  "Substract to the ENTITY's heatlh POINT health points."
  (set-current-health entity (- (get-current-health entity) points))
  (when (< (get-current-health entity) 0)
    (set-current-health entity 0)))

(defmethod heal ((entity rlk--entity) points)
  "Add to the ENTITY's current health POINT health points."
  (set-current-health entity (+ (get-current-health entity) points))
  (when (> (get-current-health entity) (get-max-health entity))
    (set-current-health entity (get-max-health entity))))

;;;;;;;;;;
;; Hero ;;
;;;;;;;;;;

(defclass rlk--entity-hero (rlk--entity)
  ((type :initform :hero
         :protection :protected)
   (max-health :initarg :max-health
               :protection :protected)
   (message-logger :initarg :message-logger
                   :protection :protected))
  "The main character in the game.")


(defmethod interact-with-cell ((hero rlk--entity-hero) dx dy)
  "Try all sort of interaction with cell at DX, DY.

If cell is accessible, will move to it.
If not, and it has a door, will open it."
  (let* ((x (+ (get-x hero) dx))
        (y (+ (get-y hero) dy))
        (cell (get-cell-at (get-grid hero) x y)))
    (if (is-accessible-p cell)
        (try-move hero dx dy)
      (when (is-container-p cell)
        (catch 'end
          (dolist (object (get-objects cell))
            (when (equal (get-type object) :door-closed)
              (do-action object hero :open)
              (display-message hero "You open the door.")
              (spend-time-delay hero 2)
              (throw 'end nil))))
        ))))

;;;;;;;;;;;;;
;; Enemies ;;
;;;;;;;;;;;;;

(defclass rlk--entity-enemy (rlk--entity)
  ()
  "Base classe for enemies."
  :abstract t)

(defmethod move-randomly ((enemy rlk--entity-enemy))
  "Try to move on a random neighbour cell.
Return t if it could move, nil otherwise."
  (let ((accessible-cells '())
        (grid (get-grid enemy))
        (choosen-cell nil))
    (dotimes (i 3)
      (dotimes (j 3)
        (let*
            ((dx (- i 1))
             (dy (- j 1))
             (x (+ (get-x enemy) (- i 1)))
             (y (+ (get-y enemy) (- j 1)))
             (cell (get-cell-at grid x y)))
          (when (is-accessible-p cell)
            (add-to-list 'accessible-cells (cons dx dy))))))
    (if accessible-cells
        (progn
          (setq choosen-cell (nth (random (length accessible-cells))
                                  accessible-cells))
          (try-move enemy (car choosen-cell) (cdr choosen-cell)))
      (spend-time-delay enemy 1))))

(defmethod update ((enemy rlk--entity-enemy))
  "Update the enemy until it has no time to spend.
Currently means moving randomly the enemy."
  (when (can-do-action enemy)
    (move-randomly enemy)))


(defclass rlk--entity-enemy-rat (rlk--entity-enemy)
  ((type :initform :rat
         :protection :protected)
   (max-health :initform 3
               :protection :protected)
   (message-logger :initarg :message-logger
                   :protection :protected))
  "Rat is the weakest enemy.")

(provide 'roguel-ike-entity)
;;; roguel-ike-entity.el ends here
