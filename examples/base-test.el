;;; base-test.el --- Simple functionnal test for roguel-ike base

;;; Commentary:

;; Simple test

;;; Code:

(require 'roguel-ike-graphics)
; @todo Missing grid parameter
(defvar cells)
(setq cells (list
             (list
              (rlk--level-cell "Cell" :type :wall :x 0 :y 0)
              (rlk--level-cell "Cell" :type :wall :x 1 :y 0)
              (rlk--level-cell "Cell" :type :wall :x 2 :y 0)
              (rlk--level-cell "Cell" :type :wall :x 3 :y 0))
             (list
              (rlk--level-cell "Cell" :type :wall :x 0 :y 1)
              (rlk--level-cell-ground "Cell" :x 1 :y 1)
              (rlk--level-cell-ground "Cell" :x 2 :y 1)
              (rlk--level-cell "Cell" :type :wall :x 3 :y 1))
             (list
              (rlk--level-cell "Cell" :type :wall :x 0 :y 2)
              (rlk--level-cell "Cell" :type :wall :x 1 :y 2)
              (rlk--level-cell "Cell" :type :wall :x 2 :y 2)
              (rlk--level-cell "Cell" :type :wall :x 3 :y 2))))

(defvar grid)
(setq grid (rlk--level-grid "Grid" :cells cells))

(defvar hero)
(setq hero (rlk--entity-hero "Hero"))

(set-cell hero (get-cell-at grid 2 1))
(defvar renderer)
(setq renderer
  (rlk--graphics-ascii-renderer "Renderer" :buffer (get-buffer-create "*rlk*")))

(draw-grid renderer grid)
;;; base-test.el ends here
