;;; effect.el --- Temporary effects on statistics

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
;;

;;; Code:
(require 'roguel-ike/entity)
(unless (require 'cl-lib nil :no-error)
  (require 'cl)
  (defalias 'cl-defun 'defun*))

(defclass rlk--stats-effect ()
  ((type :initarg :type
         :type symbol
         :reader get-type
         :protection :private
         :documentation "Identifier of the effect.")
   (name :initarg :name
         :type string
         :reader get-name
         :protection :private
         :documentation "Name of the effect.")
   (start-message :initarg :start-message
                  :type (or string list)
                  :reader get-start-message
                  :protection :private
                  :documentation "The message displayed when the effect starts.

This message will be sent to entity's method `display-message'.")
   (end-message :initarg :end-message
                  :type (or string list)
                  :reader get-end-message
                  :protection :private
                  :documentation "The message displayed when the effect ends.

This message will be sent to entity's method `display-message'.")
   (period :initarg :period
           :type integer
           :reader get-period
           :protection :private
           :documentation "The number of turn between each effect application.")
   (apply-number :initarg :apply-number
                 :type integer
                 :reader get-apply-number
                 :protection :private
                 :documentation "The number of times the effect will be applied.")
   (stats-change :initarg :stats-change
                 :type list
                 :reader get-stats-change
                 :protection :private
                 :documentation "A property list telling which stat will be impacted, and by how many.")
   (minimal-values :initarg :minimal-values
                   :type list
                   :reader get-minimal-values
                   :protection :private
                   :documentation "A property list telling the threshold for negative stats changes."))
  "Stat effect applies temporary and periodical bonuses or penalties on entities.")

;;;;;;;;;;;;;;;;;;;;;;;;
;; Effect application ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rlk--stats-effect-applier ()
  ((effect :initarg :effect
           :type rlk--stats-effect
           :reader get-effect
           :protection :private
           :documentation "The effect to apply.")
   (entity :initarg :entity
           :type rlk--entity
           :reader get-entity
           :protection :private
           :documentation "The entity on which the effect will be applied.")
   (turn-count :initform 0
               :type integer
               :protection :private
               :documentation "The number of turns since the last application.")
   (application-count :initform 0
                      :type integer
                      :protection :private
                      :documentation "The number of times the effect has been applied.")
   (turn-callback :type function
                  :protection :private
                  :documentation "The callback function called each time the entity spends a turn."))
  "Apply a stats effect to an entity.")

(defmethod initialize-instance :after ((self rlk--stats-effect-applier) slots)
  "Cache the callback function."
  (oset self turn-callback (apply-partially 'add-turns self)))

(defmethod start ((self rlk--stats-effect-applier))
  "Start applying the effect."
  (let ((entity (get-entity self)))
    (display-message entity (get-start-message (get-effect self)))
    (register (get-dispatcher entity) :turns-spent (oref self turn-callback))
    (register-effect-applier entity self)))

(defmethod stop ((self rlk--stats-effect-applier))
  "Stop applying the effect."
  (let ((entity (get-entity self)))
    (display-message entity (get-end-message (get-effect self)))
    (unregister (get-dispatcher entity) :turns-spent (oref self turn-callback))
    (unregister-effect-applier entity self)))

(defmethod reset ((self rlk--stats-effect-applier))
  "Reinitialize the counters."
  (oset self turn-count 0)
  (oset self application-count 0))

(defmethod add-turns ((self rlk--stats-effect-applier) nb-turns)
  "Add NB-TURNS to current turn count, and apply the effect if necesary."
  (let ((period (get-period (get-effect self)))
        (apply-number (get-apply-number (get-effect self))))
    (oset self turn-count (+ nb-turns (oref self turn-count)))

    (while (and (> (oref self turn-count) period)
                (< (oref self application-count) apply-number))
      (apply-effect self)
      (oset self turn-count (- (oref self turn-count) period)))

    (when (= (oref self application-count) apply-number)
      (stop self))))

(defmethod apply-effect ((self rlk--stats-effect-applier))
  "Apply the effect one time."
  (let ((stats-change (get-stats-change (get-effect self)))
        (entity (get-entity self))
        (minimal-values (get-minimal-values (get-effect self))))
    (while stats-change
      (let ((slot (get-stat-slot entity (car stats-change)))
            (minimal-value (plist-get minimal-values (car stats-change)))
            (change (cadr stats-change)))
        (when (or (> change 0)
                  (null minimal-value)
                  (> (get-current-value slot) minimal-value))
          (set-current-value slot (+ (get-current-value slot) (cadr stats-change))))
        (setq stats-change (cddr stats-change))))
    (oset self application-count (1+ (oref self application-count)))))

(defmethod apply-on ((self rlk--stats-effect) entity)
  (unless (catch 'already-applied
            (dolist (applier (get-current-effects entity))
              (when (equal (get-type self) (get-type (get-effect applier)))
                (reset applier)
                (throw 'already-applied t)))
            nil)
    (start (rlk--stats-effect-applier (format "%s effect applier" (get-name self))
                                      :effect self
                                      :entity entity))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects registering ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rlk--effects '()
  "All the define effects.")

(cl-defun rlk--defeffect (&key type
                               name
                               start-message
                               end-message
                               period
                               apply-number
                               stats-change
                               (minimal-values nil))
  "Define a new stat effect.

TYPE is the identifier of the effect.

NAME is its displayed name.

START-MESSAGE is the message displayed when the effect starts on an entity.

END-MESSAGE is the message displayed when the effect ends on an entity.

PERIOD is the number of turns between each effect application.

APPLY-NUMBER is the number of times the effect will be applied.

STATS-CHANGE is a property list containing the impacted slots, and by
how much each one will increase / decrease.

MINIMAL-VALUES is a property list containing for each impacted slot the
threshold under which a negative effect does not apply anymore."
  (add-to-list 'rlk--effects (rlk--stats-effect (format "Effect %s" name)
                                               :type type
                                               :name name
                                               :start-message start-message
                                               :end-message end-message
                                               :period period
                                               :apply-number apply-number
                                               :stats-change stats-change
                                               :minimal-values minimal-values)))

(defun rlk--effect-get-effect (type)
  "Return the effect whose type is TYPE.

Return nil when there is no such effect."
  (catch 'effect
    (dolist (effect rlk--effects)
      (when (equal type (get-type effect))
        (throw 'effect effect)))))


(provide 'roguel-ike/stats/effect)
;;; effect.el ends here
