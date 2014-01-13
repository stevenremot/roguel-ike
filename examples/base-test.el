;;; base-test.el --- Simple functionnal test for roguel-ike base

;;; Commentary:

;; Simple test

;;; Code:

(require 'roguel-ike-game)
(require 'roguel-ike-controller)

(defvar layout '("############"
                 "#..#####...#"
                 "#....#...###"
                 "###......###"
                 "############"))

(defvar cells)
(setq cells nil)

(dolist (line layout)
  (let ((cell-line '()))
    (dolist (character (split-string line "" t))
      (setq cell-line
            (append cell-line
                    (list
                     (cond ((string-equal character "#")
                            (rlk--level-cell "Wall cell" :type :wall))
                           ((string-equal character ".")
                            (rlk--level-cell-ground "Ground cell"))
                           (t
                            (rlk--level-cell "Unknown cell" :type :void)))))))
    (setq cells (append cells (list cell-line)))))

(defvar grid)
(setq grid (rlk--level-grid "Grid" :cells cells))


(defvar hero)
(setq hero (rlk--entity-hero "Hero"))
(set-grid hero grid)
(set-pos hero 1 1)

(defvar renderer)
(setq renderer
      (rlk--graphics-ascii-renderer "Renderer" :buffer (get-buffer-create "*rlk*")))

(defvar game)
(setq game (rlk--game "Game" :grid grid :hero hero))

(defvar controller)
(setq controller (rlk--controller-game "Controller" :game game :renderer renderer))

(draw-grid renderer grid)

(setup controller)
;;; base-test.el ends here
