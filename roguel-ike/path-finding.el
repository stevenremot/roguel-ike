;;; path-finding.el --- Path finding system

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
;; Implement A* algorithm to get the shortest path from
;; one point to another in a level.

;;; Code:
(require 'roguel-ike/level)
(require 'roguel-ike/level/cell)
(require 'roguel-ike-lib/math)

(defclass rlk--path-finding-node ()
  ((point :initarg :point
          :type cons
          :reader get-point
          :protection :private
          :documentation "The node's point.")
   (parent :initarg :parent
           :type (or rlk--path-finding-node null)
           :reader get-parent
           :writer set-parent
           :protection :private
           :documentation "The node's parent.

Used to keep the path in mind.")
   (partial-cost :initarg :partial-cost
                 :type integer
                 :reader get-partial-cost
                 :writer set-partial-cost
                 :protection :private
                 :documentation "The cost of the path to reach this node from the origin."))
  "A node in a considered path.")

(defconst rlk--path-finding-neighbours
  (list
   (cons 0 1)
   (cons 1 1)
   (cons 1 0)
   (cons 1 -1)
   (cons 0 -1)
   (cons -1 -1)
   (cons -1 0)
   (cons -1 1))
  "Pre-instanciated directions for neighbour cells.")

(defun rlk--path-finding-get-minimal-cons (opened-list)
  "Return the cons with minimal cost in OPENED-LIST."
  (let ((minimum-cost 0)
        (minimum-cons nil))
    (dolist (node-cons opened-list)
      (when (or (null minimum-cons)
                (< (car node-cons) minimum-cost))
        (setq minimum-cost (car node-cons)
              minimum-cons node-cons)))
    minimum-cons))

(defun rlk--path-finding-create-path-from-node (end-node)
  "Return the sequence of points representing END-NODE's path.

The sequence is ordered in the path order.

The points are conses in the form (x . y)."
  (let ((path '())
        (current-node end-node))
    (while current-node
      (let ((point (get-point current-node)))
        (setq path (cons point path)
              current-node (get-parent current-node))))
    path))

(defun rlk--path-finding-find-path (origin target level)
  "Find the shortest path fom ORIGIN to TARGET in LEVEL.

Return nil if there is not path from ORIGIN to TARGET."
    (let ((opened-list (list (cons (roguel-ike-math-get-distance origin target)
                                   (rlk--path-finding-node "Origin node"
                                                           :point origin
                                                           :parent nil
                                                           :partial-cost 0))))
        (closed-list '())
        (end-node nil))
    (while (and opened-list
                (not end-node))
      (let* ((considered-cons (rlk--path-finding-get-minimal-cons opened-list))
             (considered-node (cdr considered-cons))
             (considered-point (get-point considered-node)))
        (if (equal (get-point considered-node) target)
            (setq end-node considered-node)
          (setq opened-list (delete considered-cons opened-list))

          (dolist (neighbour-direction rlk--path-finding-neighbours)
            (let* ((neighbour-point (cons (+ (car neighbour-direction) (car considered-point))
                                          (+ (cdr neighbour-direction) (cdr considered-point))))
                   (neighbour-node (rlk--path-finding-node "Neighbour node"
                                                           :point neighbour-point
                                                           :parent considered-node
                                                           :partial-cost (1+ (get-partial-cost considered-node)))))
              (if (equal neighbour-point target)
                  (setq end-node neighbour-node)

                (when (is-accessible-p (get-cell-at level (car neighbour-point) (cdr neighbour-point)))
                  (unless (catch 'in-closed-list
                            (dolist (closed-node closed-list)
                              (when (equal neighbour-point (get-point closed-node))
                                (when
                                    (< (get-partial-cost neighbour-node) (get-partial-cost closed-node))
                                  (set-parent closed-node considered-node)
                                  (set-partial-cost closed-node (get-partial-cost neighbour-node)))
                                (throw 'in-closed-list t)))
                            nil)
                    (add-to-list 'opened-list (cons (+ (get-partial-cost neighbour-node)
                                                       (roguel-ike-math-get-distance neighbour-point target))
                                                    neighbour-node))))))))
        (add-to-list 'closed-list considered-node)))

    (rlk--path-finding-create-path-from-node end-node)))

(defun rlk--path-finding-get-direction-to-target (origin target level)
  "Return the direction ORIGIN should take to go to TARGET in LEVEL.

ORIGIN and TARGET are conses in the form (x . y).

Return nil if there is no path from ORIGIN to TARGET."
  (let* ((path (rlk--path-finding-find-path origin target level))
         (first-point (nth 1 path)))
    (if path
        (progn
          (cons (- (car first-point) (car origin))
                (- (cdr first-point) (cdr origin))))
      nil)))

(provide 'roguel-ike/path-finding)
;;; path-finding.el ends here
