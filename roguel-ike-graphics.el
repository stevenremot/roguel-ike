;;; graphics.el --- In charge of drawing things

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-entity)

(defclass rlk--graphics-ascii-renderer ()
  ((symbols-table :initarg :symbols-table
                  :initform ((:ground . ".")
                             (:wall . "#")
                             (:void . " ")
                             (:hero . "@"))
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
         (character (cdr (assoc symbol symbols))))
    (insert character)))

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
