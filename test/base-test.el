;;; base-test.el --- Simple functionnal test for roguel-ike base

;;; Commentary:

;;; Code:

(require 'roguel-ike/graphics)

(defvar cells)
(setq cells (list
             (list
              (roguel-ike-level-cell "Cell" :type :wall)
              (roguel-ike-level-cell "Cell" :type :wall)
              (roguel-ike-level-cell "Cell" :type :wall)
              (roguel-ike-level-cell "Cell" :type :wall))
             (list
              (roguel-ike-level-cell "Cell" :type :wall)
              (roguel-ike-level-cell "Cell" :type :ground)
              (roguel-ike-level-cell "Cell" :type :ground)
              (roguel-ike-level-cell "Cell" :type :wall))
             (list
              (roguel-ike-level-cell "Cell" :type :wall)
              (roguel-ike-level-cell "Cell" :type :wall)
              (roguel-ike-level-cell "Cell" :type :wall)
              (roguel-ike-level-cell "Cell" :type :wall))))

(defvar grid)
(setq grid (roguel-ike-level-grid "Grid" :cells cells))

(defvar hero)
(setq hero (roguel-ike-entity-hero "Hero"))

(set-entity (cell-at grid 2 1) hero)

(defvar symbols (make-hash-table))
(puthash :wall "#" symbols)
(puthash :ground "." symbols)
(puthash :hero "@" symbols)

(with-current-buffer (get-buffer-create "*roguel-ike*")
  (draw grid symbols))
;;; base-test.el ends here
