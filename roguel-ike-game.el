;;; roguel-ike-game.el --- Game state

;;; Commentary:
;;

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-entity)
(require 'roguel-ike-buffer)

(defclass rlk--game ()
  ((current-grid :initarg :grid
                 :type rlk--level-grid
                 :reader get-current-grid
                 :writer set-current-grid
                 :protection :private
                 :documentation "Current grid to display.")
   (hero :initarg :hero
         :type rlk--entity-hero
         :reader get-hero
         :protection :private
         :documentation "Player's character.")
   (buffer-manager :initarg :buffer-manager
                   :type rlk--buffer-manager
                   :reader get-buffer-manager
                   :protection :private
                   :documentation "Game's buffer manager."))
  "Contain the game state.")

(provide 'roguel-ike-game)

;;; roguel-ike-game.el ends here
