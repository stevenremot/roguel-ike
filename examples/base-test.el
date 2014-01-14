;;; base-test.el --- Simple functionnal test for roguel-ike base

;;; Commentary:

;; Simple test

;;; Code:

(require 'roguel-ike-game)
(require 'roguel-ike-controller)
(require 'roguel-ike-buffer)
(require 'roguel-ike-message)

(defvar buffer-manager (rlk--buffer-manager "Buffer manager"))
(defvar message-logger (rlk--message-logger "Message logger"
                                            :message-buffer (get-message-buffer buffer-manager)))

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
(setq hero (rlk--entity-hero "Hero" :max-health 10))
(set-grid hero grid)
(set-pos hero 1 1)

(defvar rat)
(setq rat (rlk--entity-enemy-rat "Rat"))
(set-grid rat grid)
(set-pos rat 9 1)

(defvar renderer)
(setq renderer
      (rlk--graphics-renderer-game "Renderer"
                                   :buffer (get-game-buffer buffer-manager)))

(defvar game)
(setq game (rlk--game "Game" :grid grid :hero hero))

(defvar controller)
(setq controller (rlk--controller-game "Controller" :game game :renderer renderer))

(defvar stats-renderer (rlk--graphics-renderer-stats "Statistics"
                                                     :hero hero
                                                     :buffer (get-stats-buffer buffer-manager)))

(setup-layout buffer-manager)
(draw-stats stats-renderer)

(display-message message-logger "Welcome, young adventurer !")

(draw-grid renderer grid)

(setup controller)
;;; base-test.el ends here
