;;; roguel-ike.el --- Main file for roguel-ike

;;; Commentary:

;; Contains launch command

;;; Code:

(require 'roguel-ike-game)
(require 'roguel-ike-buffer)
(require 'roguel-ike-message)
(require 'roguel-ike-graphics)
(require 'roguel-ike-controller)
(require 'roguel-ike-interactive-object)

(defun rlk--get-cells-from-layout (layout)
  "Create a cell grid from a LAYOUT, a list of string representing the level."
  (let ((cells '()))
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
    cells))

(defun roguel-ike ()
  "Start a roguel-ike game."
  (interactive)
  (let* ((buffer-manager (rlk--buffer-manager "Buffer manager"))
         (layout '("############"
                   "#..#####...#"
                   "#....#...###"
                   "###......###"
                   "############"))
         (cells (rlk--get-cells-from-layout layout))
         (grid (rlk--level-grid "Grid" :cells cells))
         (hero (rlk--entity-hero "Hero" :max-health 10))
         (rat (rlk--entity-enemy-rat "Single rat")) ;; TODO replace this by a monster dropper
         (door (rlk--interactive-object-door "Door"))
         (game (rlk--game "Game"
                          :grid grid
                          :hero hero
                          :buffer-manager buffer-manager))
         (stats-renderer (rlk--graphics-renderer-stats "Stats renderer"
                                                       :buffer (get-stats-buffer buffer-manager)
                                                       :hero hero))
         (message-logger (rlk--message-logger "Message logger"
                                              :message-buffer (get-message-buffer buffer-manager)))
         (game-renderer (rlk--graphics-renderer-game "Game renderer"
                                                     :buffer (get-game-buffer buffer-manager)))
         (game-controller (rlk--controller-game "Game controller"
                                                :game game
                                                :renderer game-renderer)))
    (set-grid hero grid)
    (set-pos hero 1 1)
    (set-grid rat grid)
    (set-pos rat 9 1)

    (add-object (get-cell-at grid 5 3) door)

    (setup-layout buffer-manager)
    (draw-stats stats-renderer)
    (display-message message-logger "Welcome, young adventurer!")
    (draw-grid game-renderer grid)
    (setup game-controller)))

(provide 'roguel-ike)

;;; roguel-ike.el ends here
