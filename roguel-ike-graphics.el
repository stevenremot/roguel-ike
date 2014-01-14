;;; graphics.el --- In charge of drawing things

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-entity)

;;;;;;;;;;;;;;;;
;; Game faces ;;
;;;;;;;;;;;;;;;;

(defgroup rlk-faces
  '()
  "Group for rlk faces"
  :group 'emacs)

(defface rlk-face-default
  '()
  "Standard roguel-ike face"
  :group 'rlk-faces)

(defface rlk-face-wall
  '((((class color) (min-colors 88))
      :inherit 'rlk-face-default))
  "Wall face"
  :group 'rlk-faces)

(defface rlk-face-ground
  '((((class color) (min-colors 88))
      :inherit 'rlk-face-default
      :foreground "gray8"))
  "Ground face"
  :group 'rlk-faces)

(defface rlk-face-hero
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "yellow3"))
  "Hero face"
  :group 'rlk-faces)

(defface rlk-face-rat
  '((((class color) (min-colors 8))
    :inherit 'rlk-face-default
    :foreground "red"))
  "Rat face"
  :group 'rlk-faces)

;;;;;;;;;;;;;;
;; Renderer ;;
;;;;;;;;;;;;;;

(defclass rlk--graphics-ascii-renderer ()
  ((symbols-table :initarg :symbols-table
                  :initform ((:ground . ("." . rlk-face-ground))
                             (:wall  . ("#" . rlk-face-wall))
                             (:void . (" " . rlk-face-default))
                             (:hero . ("@" . rlk-face-hero))
                             (:rat . ("r" . rlk-face-rat)))
                  :accessor get-symbols-table
                  :type (or list symbol)
                  :protection :private
                  :documentation "The mapping between object type and ASCII symbol.

See rlk--graphics-ascii-symbol-table for the format.")
   (buffer :initarg :buffer
           :type buffer
           :accessor get-target-buffer
           :protection :private
           :documentation "The buffer on which the level will be rendered."))
  "Renderer for game level")

;; TODO refine it with is-lit-p
(defmethod draw-cell ((renderer rlk--graphics-ascii-renderer) cell)
  "Draws the cell on the current buffer, at the current position

  symbols is a hash table whose keys are cell types, and values are
  corresponding symbols"
  (let* ((symbol (if (has-entity-p cell)
                     (get-type (get-entity cell))
                   (get-type cell)))
         (symbols (get-symbols-table renderer))
         (parameters (cdr (assoc symbol symbols)))
         (character (car parameters))
         (face (cdr parameters)))
    (insert (propertize character 'face face))))

(defmethod draw-grid ((renderer rlk--graphics-ascii-renderer) grid)
  "Draws the grid on the current buffer

  symbols is the hash table with cell types as key and characters
  as values"
  (with-current-buffer (get-target-buffer renderer)
    (erase-buffer)
    (dolist (line (get-cells grid))
      (dolist (cell line)
        (draw-cell renderer cell))
      (insert "\n"))))

(provide 'roguel-ike-graphics)
;;; roguel-ike-graphics.el ends here
