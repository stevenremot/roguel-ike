;;; graphics.el --- In charge of drawing things

;;; Commentary:

;;; Code:

(require 'eieio)
(require 'roguel-ike-level)
(require 'roguel-ike-entity)

(defvar rlk-graphics-ascii-symbol-table
  '((:ground . ".")
    (:wall . "#")
    (:void . " ")
    (:hero . "@"))
  "Alist associating types to ASCII symbols.")

(defclass rlk-graphics-ascii-renderer ()
  ((symbols-table :initarg :symbols-table
                  :initform rlk-graphics-ascii-symbol-table
                  :type (or list symbol)
                  :protection :private
                  :documentation "The mapping between object type and ASCII symbol.

See rlk-graphics-ascii-symbol-table for the format.")
   (buffer :initarg :buffer
           :type buffer
           :protection :private
           :documentation "The buffer on which the level will be rendered."))
  "Renderer for game level")

(defmethod get-symbols-table ((renderer rlk-graphics-ascii-renderer))
  "Returns the symbols table of the renderer"
  (let ((slot (oref renderer symbols-table)))
    (if (listp slot)
        slot
      (symbol-value slot))))

(defmethod draw-cell ((renderer rlk-graphics-ascii-renderer) cell)
  "Draws the cell on the current buffer, at the current position

  symbols is a hash table whose keys are cell types, and values are
  corresponding symbols"
  (let* ((symbol (if (and
                      (is-lit-p cell)
                      (has-entity-p cell))
                     (get-type (get-entity cell))
                   (get-type cell)))
         (symbols (get-symbols-table renderer))
         (character (cdr (assoc symbol symbols))))
    (insert character)))

(defmethod draw-grid ((renderer rlk-graphics-ascii-renderer) grid)
  "Draws the grid on the current buffer

  symbols is the hash table with cell types as key and characters
  as values"
  (with-current-buffer (oref renderer buffer)
    (erase-buffer)
    (dolist (line (get-cells grid))
      (dolist (cell line)
        (draw-cell renderer cell))
      (insert "\n"))))

(provide 'roguel-ike-graphics)
;;; roguel-ike-graphics.el ends here
