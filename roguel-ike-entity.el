;;; entity.el --- Manages game entities

;; Copyright (C) 2014 Steven Rémot

;;; Author: Steven Rémot

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Define roguel-ike moving entities

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-message)
(require 'roguel-ike-interactive-object)

;;;;;;;;;;;;;;;;
;; Statistics ;;
;;;;;;;;;;;;;;;;

(defclass rlk--entity-stat-slot ()
  ((max-value :initarg :max-value
              :type number
              :reader get-max-value
              :writer set-max-value
              :protection :private
              :documentation "The maximum value of the statistic.")
   (current-value :type number
                  :protection :private
                  :documentation "The current value of the statistic.
Cannot be out of the range 0 - max-value."))
  "Statistic slot.
Handle maximum value and current value.")

(defmethod get-current-value ((self rlk--entity-stat-slot))
  "Return the current slot value.
Set it to max-value if not set yet."
  (unless (slot-boundp self 'current-value)
    (set-current-value self (get-max-value self)))
  (oref self current-value))

(defmethod set-current-value ((self rlk--entity-stat-slot) current-value)
  "Set the current slot value.
Restrain it to the range 0 - max-value."
  (oset self current-value
        (cond ((< current-value 0)
               0)
              ((> current-value (get-max-value self))
               (get-max-value self))
              (t
               current-value))))

(defclass rlk--entity-stats ()
  ((slots :initarg :slots
          :type list
          :protection :private
          :documentation "An associated list whose keys are slot names and values are slots."))
   "Entity's statistics.")

(defmethod initialize-instance ((self rlk--entity-stats) slots)
  "Initialize the slots."
  (let ((stat-slots '())
        (slot-names '(:health
                      :stamina
                      :strength
                      :constitution
                      :speed
                      :spirit)))
    (dolist (name slot-names)
      (add-to-list 'stat-slots
                   (cons name
                         (rlk--entity-stat-slot (format "%s slot" name)
                                                :max-value (plist-get  slots name)))))
  (call-next-method self (list :slots stat-slots))))

(defmethod get-slot ((self rlk--entity-stats) slot)
  "Return the slot named SLOT."
  (cdr (assoc slot (oref self slots))))

;;;;;;;;;;;;;;;;;;;;;
;; Abstract entity ;;
;;;;;;;;;;;;;;;;;;;;;

(defclass rlk--entity (rlk--level-cell-object)
  ((stats :initarg :stats
         :type rlk--entity-stats
         :reader get-stats
         :protection :protected
         :documentation "Entity's statistics.")
   (time-delay :initform 0
               :type integer
               :reader get-time-delay
               :protection :protected
               :documentation "The amount of time the entity have to wait / spend.")
   (x :initform -1
      :type integer
      :reader get-x
      :protection :private
      :documentation "The horizontal position of the entity in the level.")
   (y :initform -1
      :type integer
      :reader get-y
      :protection :private
      :documentation "The vertical position of the entity in the level.")
   (level :initform nil
          :type (or rlk--level boolean)
          :reader get-level
          :writer set-level
          :protection :private
          :documentation "The level which contains the entity.")
   (message-logger :type rlk--message-logger
                   :reader get-message-logger
                   :protection :protected
                   :documentation "The logger used to display entitie's messages."))
  "The base class for game entities."
  :abstract t)

(defmethod get-layer ((self rlk--entity))
  "See rlk--level-cell-object."
  3)

(defmethod is-entity-p ((self rlk--entity))
  "See rlk--level-cell-object."
  t)

(defmethod accept-other-object-p ((self rlk--entity))
  "See rlk--level-cell-object."
  nil)

(defmethod display-message ((self rlk--entity) message)
  "Use the message logger to display a message."
  (display-message (get-message-logger self) message))

(defmethod add-time-delay ((self rlk--entity) time)
  "Add TIME to time-delay."
  (oset self time-delay (- (get-time-delay self) time)))

(defmethod spend-time-delay ((self rlk--entity) time)
  "Subtract TIME to time-delay."
  (oset self time-delay (+ (get-time-delay self) time)))

(defmethod can-do-action ((self rlk--entity))
  "Return t is the entity can do an action, nil otherwise."
  (< (get-time-delay self) 0))

(defmethod get-cell ((self rlk--entity))
  "Return the cell on which stands the entity."
  (get-cell-at (get-level self)
               (get-x self)
               (get-y self)))


(defmethod set-cell ((self rlk--entity) cell)
  "Set the new cell of the entity.
This method is instead for private use ONLY.
If you want to change entity position, use set-pos instead."
  (let ((old-cell (get-cell self)))
    (when (rlk--level-cell-ground-child-p old-cell)
      (set-entity old-cell nil))
    (set-entity cell self)))


(defmethod set-pos ((self rlk--entity) x y)
  "Set the new cell pos."
  (let ((cell (get-cell-at (get-level self) x y)))
    (when cell
      (set-cell self cell)
      (oset self x x)
      (oset self y y))))

(defmethod try-move ((self rlk--entity) dx dy)
  "If the entity can move to the cell (x + DX, y + DY), will move to it.
Return t if the entity could move, nil otherwise."
  (let* ((x (+ (get-x self) dx))
        (y (+ (get-y self) dy))
        (cell (get-cell-at (get-level self) x y)))
    (if (and cell (is-accessible-p cell))
        (prog2
            (set-pos self x y)
            (spend-time-delay self 1)
            t))
    nil))

(defmethod entity-alive-p ((self rlk--entity))
  "Return t if the entity is alive, nil otherwise."
  (> (get-current-health self) 0))

;;;;;;;;;;;;;;;;;;;;;;
;; Slot interaction ;;
;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-stat-slot ((self rlk--entity) slot)
  "Return the stat slot SLOT."
  (get-slot (get-stats self) slot))

(defmethod get-max-health ((self rlk--entity))
  "Return the maximum health the entity can have."
  (get-max-value (get-stat-slot self :health)))

(defmethod get-current-health ((self rlk--entity))
  "Return the entity's current health."
  (get-current-value (get-stat-slot self :health)))

(defmethod set-current-health ((self rlk--entity) health)
  "Set entity's current health to HEALTH."
  (set-current-value (get-stat-slot self :health) health))

(defmethod hurt ((self rlk--entity) points)
  "Substract to the entity's heatlh POINT health points."
  (set-current-health self (- (get-current-health self) points))
  (when (< (get-current-health self) 0)
    (set-current-health self 0)))

(defmethod heal ((self rlk--entity) points)
  "Add to the entity's current health POINT health points."
  (set-current-health self (+ (get-current-health self) points))
  (when (> (get-current-health self) (get-max-health self))
    (set-current-health self (get-max-health self))))

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


(defmethod interact-with-cell ((self rlk--entity-hero) dx dy)
  "Try all sort of interaction with cell at DX, DY.

If cell is accessible, will move to it.
If not, and it has a door, will open it."
  (let* ((x (+ (get-x self) dx))
        (y (+ (get-y self) dy))
        (cell (get-cell-at (get-level self) x y)))
    (if (is-accessible-p cell)
        (try-move self dx dy)
      (when (is-container-p cell)
        (catch 'end
          (dolist (object (get-objects cell))
            (when (equal (get-type object) :door-closed)
              (do-action object self :open)
              (display-message self "You open the door.")
              (spend-time-delay self 2)
              (throw 'end nil))))
        ))))

;;;;;;;;;;;;;
;; Enemies ;;
;;;;;;;;;;;;;

(defclass rlk--entity-enemy (rlk--entity)
  ()
  "Base classe for enemies."
  :abstract t)

(defmethod set-level ((self rlk--entity-enemy) level)
  "See rlk--entity.
Register/unregister SELF to the new and old levels too."
  (let ((old-level (get-level self)))
    (when old-level
      (remove-enemy old-level self)))
  (call-next-method)
  (add-enemy level self))

(defmethod move-randomly ((self rlk--entity-enemy))
  "Try to move on a random neighbour cell.
Return t if it could move, nil otherwise."
  (let ((accessible-cells '())
        (level (get-level self))
        (choosen-cell nil))
    (dotimes (i 3)
      (dotimes (j 3)
        (let*
            ((dx (- i 1))
             (dy (- j 1))
             (x (+ (get-x self) (- i 1)))
             (y (+ (get-y self) (- j 1)))
             (cell (get-cell-at level x y)))
          (when (is-accessible-p cell)
            (add-to-list 'accessible-cells (cons dx dy))))))
    (if accessible-cells
        (progn
          (setq choosen-cell (nth (random (length accessible-cells))
                                  accessible-cells))
          (try-move self (car choosen-cell) (cdr choosen-cell)))
      (spend-time-delay self 1))))

(defmethod update ((self rlk--entity-enemy))
  "Update the enemy until it has no time to spend.
Currently means moving randomly the enemy."
  (when (can-do-action self)
    (move-randomly self)))


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
