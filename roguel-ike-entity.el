;;; entity.el --- Manages game entities

;;; Commentary:

;;; Code:

(require 'eieio)

(defclass rlk-entity ()
  ((type :initarg :type
         :type symbol
         :accessor get-type
         :write set-type
         :documentation "Type of the entity"))
  "Base class for game entities"
  :abstract t)

(defclass rlk-entity-hero (rlk-entity)
  ((type :initform :hero))
  "Main character in the game")

(provide 'roguel-ike-entity)
;;; roguel-ike-entity.el ends here
