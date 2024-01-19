;;; binary-node.el --- Binary tree node for dungeon generation

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
(require 'cl-generic)
(require 'eieio)

(defclass roguel-ike-level-generator-binary-node ()
  ((x :initarg :x
      :type integer
      :reader get-x
      :protection :private
      :documentation "The left of the square.")
   (y :initarg :y
      :type integer
      :reader get-y
      :protection :private
      :documentation "The top of the square.")
   (width :initarg :width
          :type integer
          :reader get-width
          :protection :private
          :documentation "The horizontal number of cells in the square.")
   (height :initarg :height
           :type integer
           :reader get-height
           :protection :private
           :documentation "The vertical number of cells in the square.")
   (first-child :type roguel-ike-level-generator-binary-node
                :reader get-first-child
                :protection :private
                :documentation "The first child of the node.

It is either at the top or at left of the square.")
   (second-child :type roguel-ike-level-generator-binary-node
                 :reader get-second-child
                 :protection :private
                 :documentation "The second child of the node.

It is either at the bottom or at right of the square."))
  "A binary tree node representing a square in a level.

This node, if big enough, can be split horizontally or vertically on two
sub squares, which will be represented by its child nodes.")

(cl-defmethod is-leaf-p ((self roguel-ike-level-generator-binary-node))
  "Return t if it is a leaf node."
  (not (slot-boundp self 'first-child)))

(cl-defmethod try-split ((self roguel-ike-level-generator-binary-node) minimal-size maximal-size split-probability)
  "May recursively split the node's square in two and use these two squares as children.

The split can be operated either horizontaly or vertically.

MINIMAL-SIZE is the minimal width and a height a node must have. If it is
impossible to split the square in a direction without create a square with a size
less than MINIMUM-SIZE, then the square won't be split.

MAXIMAL-SIZE is the maximum width and height a leaf can have. If the node has a
width or height greater than MAXIMAL-SIZE, it will be forced to be split.

SPLIT PROBABILITY is, when there is the choice, the probability for the square
to be split. This is a number between 0 and 100. 0 means it will never split,
100 means it will always split.

Apart from these two constraints, try-split operates with randomness, and won't
always try to split as much as possible."
  (let* ((width (get-width self))
         (height (get-height self))
         (horizontal-priority (cond ((< width (1+ (* 2 minimal-size))) -1)
                                    ((> width maximal-size) 1)
                                    (t 0)))
         (vertical-priority (cond ((< height (1+ (* 2 minimal-size))) -1)
                                  ((> height maximal-size) 1)
                                  (t 0)))
         split-direction)
    (when (> (max horizontal-priority vertical-priority) -1)
      (setq split-direction (cond ((and (= horizontal-priority 1)
                                        (= vertical-priority 1))
                                   (if (< (random 2) 0)
                                       :horizontal
                                     :vertical))
                                  ((= horizontal-priority 1)
                                   :horizontal)
                                  ((= vertical-priority 1)
                                   :vertical)
                                  ((>= (random 100) split-probability)
                                   :none)
                                  ((= horizontal-priority -1)
                                   :vertical)
                                  ((= vertical-priority -1)
                                   :horizontal)
                                  (t
                                   (if (= 0 (random 2))
                                       :horizontal
                                     :vertical))))
      (unless (eq :none split-direction)
        (split self split-direction minimal-size)
        (try-split (get-first-child self) minimal-size maximal-size split-probability)
        (try-split (get-second-child self) minimal-size maximal-size split-probability)))))

(cl-defmethod split ((self roguel-ike-level-generator-binary-node) direction minimal-size)
  "Split the square's node, using the two subsquares as children.

DIRECTION is either :horizontal or :vertical.

MINIMAL-VALUE is the minimum width and height each node must have."
  (let* ((width (get-width self))
         (height (get-height self))
         (maximal-size (- (if (eq direction :horizontal)
                              width
                            height)
                          minimal-size))
         (choosen-size (+ minimal-size (random (1+ (- maximal-size minimal-size)))))
         (first-x (get-x self))
         (first-y (get-y self))
         first-width first-height
         second-x second-y
         second-width second-height)
    (if (eq direction :horizontal)
        (setq first-width choosen-size
              first-height height
              second-x (+ first-x first-width 1)
              second-y first-y
              second-width (- width first-width 1)
              second-height height)
      (setq first-width width
            first-height choosen-size
            second-x first-x
            second-y (+ first-y first-height 1)
            second-width width
            second-height (- height first-height 1)))
    (oset self first-child (roguel-ike-level-generator-binary-node "Binary node"
                                                                   :x first-x
                                                                   :y first-y
                                                                   :width first-width
                                                                   :height first-height))
    (oset self second-child (roguel-ike-level-generator-binary-node "Binary node"
                                                                    :x second-x
                                                                    :y second-y
                                                                    :width second-width
                                                                    :height second-height))))

(cl-defmethod apply-to-layout ((self roguel-ike-level-generator-binary-node) layout)
  "Recursively draw the level to LAYOUT."
  (if (is-leaf-p self)
      (progn
        (dotimes (dx (get-width self))
          (dotimes (dy (get-height self))
            (setf (elt (elt layout (+ (get-y self) dy)) (+ (get-x self) dx)) :ground))))
    (let* ((first-child (get-first-child self))
           (second-child (get-second-child self))
           (is-horizontal (< (get-x first-child) (get-x second-child)))
           (x (cond (is-horizontal (+ (get-x first-child) (get-width first-child)))
                    (t (+ (get-x self) (random (get-width self))))))
           (y (cond (is-horizontal (+ (get-y self) (random (get-height self))))
                    (t (+ (get-y first-child) (get-height first-child))))))
      (apply-to-layout first-child layout)
      (apply-to-layout second-child layout)
      (setf (elt (elt layout y) x) :door))))

(provide 'roguel-ike-lib/level/generator/binary-node)

;;; binary-node.el ends here
