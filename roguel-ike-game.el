;;; roguel-ike-game.el --- Game state

;;; Commentary:
;;

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-entity)

(defclass rlk--game ()
  ((current-grid :initarg :grid
                 :type rlk--level-grid
                 :accessor get-current-grid
                 :writer set-current-grid
                 :protection :private
                 :documentation "Current grid to display.")
   (hero :initarg :hero
         :type rlk--entity-hero
         :accessor get-hero
         :protection :private
         :documentation "Player's character."))
  "Contain the game state.")

(provide 'roguel-ike-game)

;;; roguel-ike-game.el ends here
