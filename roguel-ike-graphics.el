;;; graphics.el --- In charge of drawing things

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-entity)

(defmethod draw ((cell roguel-ike-level-cell) symbols)
  "Draws the cell on the current buffer, at the current position

  symbols is a hash table whose keys are cell types, and values are
  corresponding symbols"
  (let* ((symbol (if (has-entity-p cell)
                     (get-type (get-entity cell))
                   (get-type cell)))
         (character (gethash symbol symbols " ")))
    (insert character)))

(defmethod draw ((grid roguel-ike-level-grid) symbols)
  "Draws the grid on the current buffer

  symbols is the hash table with cell types as key and characters
  as values"
  (erase-buffer)
  (dolist (line (get-cells grid))
    (dolist (cell line)
      (draw cell symbols))
    (insert "\n")))

(provide 'roguel-ike-graphics)
;;; roguel-ike-graphics.el ends here
