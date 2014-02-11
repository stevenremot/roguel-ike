;;; world.el --- Physical world

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
;; A physical world computes a set of motions.

;;; Code:
(require 'roguel-ike/physics/motion)

(defclass rlk--physics-world ()
  ((motions :initform ()
            :type list
            :reader get-motions
            :protection :private
            :documentation "World's motions."))
  "Aggregate and update motions.")

(defmethod add-motion ((self rlk--physics-world) motion)
  "Add a MOTION to the world.

Won't accept motion with null or negative energy."
  (when (or (null (get-energy motion))
            (> (get-energy motion) 0))
    (let ((motions (get-motions self)))
      (oset self motions (add-to-list 'motions motion)))))

(defmethod remove-motion ((self rlk--physics-world) motion)
  "Remove a MOTION from the world."
  (let ((new-motions '()))
    (dolist (old-motion (get-motions self))
      (unless (equal motion old-motion)
        (add-to-list 'new-motions old-motion)))
    (oset self motions new-motions)))

(defmethod do-step ((self rlk--physics-world))
  "Update MOTIONS for one turn.
Return t if at least one MOTION remains, nil otherwise."
  (let ((motions (get-motions self)))
    (dolist (motion motions)
      (update motion)
      (when (and (get-energy motion)
                 (= (get-energy motion) 0))
        (remove-motion self motion)))
    (> (length (get-motions self)) 0)))

(defmethod run ((self rlk--physics-world) draw-callback)
  "Update bodies as long as at least one is moving.
DRAW-CALLBACK is called at each iteration."
  (when (do-step self)
    (funcall draw-callback)
    (sleep-for 0 10)
    (redisplay)
    (run self draw-callback)))

(provide 'roguel-ike/physics/world)

;;; world.el ends here
