;;; graphics.el --- In charge of drawing things

;;; Commentary:

;; In charge of roguel-ike rendering

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
      :foreground "dark gray"))
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

(defface rlk-face-good-stat
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "green"))
  "Good statistic face"
  :group 'rlk-faces)

(defface rlk-face-average-stat
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "yellow"))
  "Average statistic face"
  :group 'rlk-faces)

(defface rlk-face-bad-stat
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "red"))
  "Ba sstatistic face"
  :group 'rlk-faces)

;;;;;;;;;;;;;;;;;;;
;; Game Renderer ;;
;;;;;;;;;;;;;;;;;;;

(defclass rlk--graphics-renderer-game ()
  ((symbols-table :initarg :symbols-table
                  :initform ((:ground . ("." . rlk-face-ground))
                             (:wall  . ("#" . rlk-face-wall))
                             (:door-opened . ("~" . rlk-face-default))
                             (:door-closed . ("+" . rlk-face-default))
                             (:void . (" " . rlk-face-default))
                             (:hero . ("@" . rlk-face-hero))
                             (:rat . ("r" . rlk-face-rat)))
                  :reader get-symbols-table
                  :type (or list symbol)
                  :protection :private
                  :documentation "The mapping between object type and ASCII symbol.

See rlk--graphics-ascii-symbol-table for the format.")
   (buffer :initarg :buffer
           :type buffer
           :reader get-target-buffer
           :protection :private
           :documentation "The buffer on which the level will be rendered."))
  "Renderer for game level")

;; TODO refine it with is-lit-p
(defmethod draw-cell ((renderer rlk--graphics-renderer-game) cell)
  "Draw the cell on the current buffer, at the current position.
symbols is a hash table whose keys are cell types, and values are
corresponding symbols."
  (let* ((symbol (if (and (is-container-p cell)
                          (get-highest-layer-object cell))
                      (get-type (get-highest-layer-object cell))
                   (get-type cell)))
         (symbols (get-symbols-table renderer))
         (parameters (cdr (assoc symbol symbols)))
         (character (car parameters))
         (face (cdr parameters)))
    (insert (propertize character 'face face))))

(defmethod draw-grid ((renderer rlk--graphics-renderer-game) grid)
  "Draw the grid on the current buffer.
Symbols is the hash table with cell types as key and characters
as values."
  (with-current-buffer (get-target-buffer renderer)
    (erase-buffer)
    (dolist (line (get-cells grid))
      (dolist (cell line)
        (draw-cell renderer cell))
      (insert "\n"))))

;;;;;;;;;;;;;;;;;;;;
;; Stats renderer ;;
;;;;;;;;;;;;;;;;;;;;

(defclass rlk--graphics-renderer-stats ()
  ((hero :initarg :hero
         :type rlk--entity-hero
         :protection :private
         :documentation "Hero whise statistics are rendered.")
   (buffer :initarg :buffer
           :type buffer
           :protection :private
           :documentation "Buffer on which statistcis are renderered."))
  "Render hero statistics")

(defmethod get-stat-face ((renderer rlk--graphics-renderer-stats) stat max-stat)
  "Return the stat's face according to its levelr elatively to te maximum of the stat."
  (let ((ratio (/ stat max-stat)))
    (cond
     ((> ratio 0.75) 'rlk-face-good-stat)
     ((> ratio 0.25) 'rlk-face-average-stat)
     (t 'rlk-face-bad-stat))))

(defmethod draw-stat ((renderer rlk--graphics-renderer-stats) stat max-stat)
  "Draw the statistic."
  (insert (propertize (format "%d/%d" stat max-stat) 'face (get-stat-face renderer stat max-stat))))

(defmethod draw-stats ((renderer rlk--graphics-renderer-stats))
  "Draw hero statistics on the buffer"
  (let
      ((hero (oref renderer hero)))
    (with-current-buffer (oref renderer buffer)
      (erase-buffer)
      (insert (propertize "Health : " 'face 'rlk-face-default))
      (draw-stat renderer (get-current-health hero) (get-max-health hero)))))

(provide 'roguel-ike-graphics)
;;; roguel-ike-graphics.el ends here
