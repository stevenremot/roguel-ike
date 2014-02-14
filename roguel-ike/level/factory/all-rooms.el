;;; all-rooms.el --- Create a dungeon with contiguous rooms

;;; Commentary:
;;

;;; Code:
(require 'roguel-ike-lib/level/generator/binary-node)
(require 'roguel-ike/level/factory/layout)

(defun rlk--level-create-all-rooms (width
                                    height
                                    minimum-size
                                    maximum-size
                                    split-probability)
  "Create a level composed of contiguous rooms.

WIDTH is the width of the level.

HEIGHT is the height of the level.

MINIMUM-SIZE is the minimum width and height a room can have.

MAXIMUM-SIZE is the maximum width ad height a room can have.

SPLIT-PROBABILITY is specific to the algorithm used to generate rooms.
It considers the whole level and split it in two parts recursively.
SPLIT-PROBABILITY is a number between 0 and 100 that tell the algorithm
to do or not to do the smallest rooms possible.  100 means always trying
to do the smallest rooms, 0 will just return one big room."
  (let ((node (roguel-ike-level-generator-binary-node "Binary node"
                                                      :x 0
                                                      :y 0
                                                      :width width
                                                      :height height))
        (layout (make-vector height nil))
        (frame-layout (make-vector (+ 2 height) nil)))
    (dotimes (y height)
      (setf (elt layout y) (make-vector width :wall)))

    (try-split node minimum-size maximum-size split-probability)
    (apply-to-layout node layout)

    (dotimes (y (+ 2 height))
      (setf (elt frame-layout y) (make-vector (+ 2 width) :wall)))

    (dotimes (y height)
      (dotimes (x width)
        (setf (elt (elt frame-layout (1+ y)) (1+ x)) (elt (elt layout y) x))))

    (rlk--level-create-from-layout frame-layout)))

(provide 'roguel-ike/level/factory/all-rooms)

;;; all-rooms.el ends here
