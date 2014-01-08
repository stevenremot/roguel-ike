;;; base-test.el --- Simple functionnal test for roguel-ike base

;;; Commentary:

;; Simple test

;;; Code:

(require 'roguel-ike-graphics)

(defvar cells)
(setq cells (list
             (list
              (rlk-level-cell "Cell" :type :wall)
              (rlk-level-cell "Cell" :type :wall)
              (rlk-level-cell "Cell" :type :wall)
              (rlk-level-cell "Cell" :type :wall))
             (list
              (rlk-level-cell "Cell" :type :wall)
              (rlk-level-cell "Cell" :type :ground)
              (rlk-level-cell "Cell" :type :ground)
              (rlk-level-cell "Cell" :type :wall))
             (list
              (rlk-level-cell "Cell" :type :wall)
              (rlk-level-cell "Cell" :type :wall)
              (rlk-level-cell "Cell" :type :wall)
              (rlk-level-cell "Cell" :type :wall))))

(defvar grid)
(setq grid (rlk-level-grid "Grid" :cells cells))

(defvar hero)
(setq hero (rlk-entity-hero "Hero"))

(set-entity (get-cell-at grid 2 1) hero)
(defvar renderer)
(setq renderer
  (rlk-graphics-ascii-renderer "Renderer" :buffer (get-buffer-create "*rlk*")))

(draw-grid renderer grid)
;;; base-test.el ends here
