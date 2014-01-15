;;; roguel-ike-controller.el --- Controllers' code

;;; Commentary:

;; Define roguel-ike controls system

;;; Code:

(require 'eieio)
(require 'roguel-ike-game)
(require 'roguel-ike-graphics)

(defvar-local rlk-controller nil
  "Game controller associated to the buffer.")

(defclass rlk--controller-game ()
  ((game :initarg :game
         :type rlk--game
         :reader get-game
         :protection :private
         :documentation "Game state.")
   (renderer :initarg :renderer
             :type rlk--graphics-renderer-game
             :reader get-renderer
             :protection :private
             :documentation "Level renderer.")
   (key-bindings :initarg :key-bindings
                 :initform (("h" . move-left)
                            ("j" . move-down)
                            ("k" . move-up)
                            ("l" . move-right)
                            ("y" . move-left-up)
                            ("b" . move-left-down)
                            ("u" . move-right-up)
                            ("n" . move-right-down)
                            ("q" . quit-rlk))
                 :type list
                 :reader get-key-bindings
                 :protection :private
                 :documentation "Game controls."))
  "In-game controller.")

(defmacro rlk--defcommand (name args docstring &rest body)
  "Create a method for a controller, and create the associated command.
The method is named NAME, and the command rlk-command-NAME.
The kind of controller on which it should be applied is specified
by ARGS
DOCSTRING is the documentation fo the method.
BODY is the method definition."
  (declare (indent defun))
  (list
   'progn
   (append (list 'defmethod name args docstring) body)
   (list 'defun
         (intern (concat "rlk-command-" (symbol-name name)))
         '()
         docstring
         '(interactive)
         (list name 'rlk-controller))
   ))

(defmethod get-hero ((controller rlk--controller-game))
  "Return the hero in the game associated to the CONTROLLER."
  (get-hero (get-game controller)))

(defmethod call-renderer ((controller rlk--controller-game))
  "Ask the renderer to render game's grid."
  (draw-grid (get-renderer controller)
             (get-current-grid (get-game controller))))

(rlk--defcommand move-left ((controller rlk--controller-game))
  "Move the hero left."
  (interact-with-cell (get-hero controller) -1 0)
  (call-renderer controller))

(rlk--defcommand move-right ((controller rlk--controller-game))
  "Move the hero right."
  (interact-with-cell (get-hero controller) 1 0)
  (call-renderer controller))

(rlk--defcommand move-up ((controller rlk--controller-game))
  "Move the hero up."
  (interact-with-cell (get-hero controller) 0 -1)
  (call-renderer controller))

(rlk--defcommand move-down ((controller rlk--controller-game))
  "Move the hero down."
  (interact-with-cell (get-hero controller) 0 1)
  (call-renderer controller))

(rlk--defcommand move-left-up ((controller rlk--controller-game))
  "Move the hero left-up."
  (interact-with-cell (get-hero controller) -1 -1)
  (call-renderer controller))

(rlk--defcommand move-left-down ((controller rlk--controller-game))
  "Move the hero left-down."
  (interact-with-cell (get-hero controller) -1 1)
  (call-renderer controller))

(rlk--defcommand move-right-up ((controller rlk--controller-game))
  "Move the hero right-up."
  (interact-with-cell (get-hero controller) 1 -1)
  (call-renderer controller))

(rlk--defcommand move-right-down ((controller rlk--controller-game))
  "Move the hero right-down."
  (interact-with-cell (get-hero controller) 1 1)
  (call-renderer controller))

(rlk--defcommand quit-rlk ((controller rlk--controller-game))
  "Quit roguel-ike."
  (kill-buffers (get-buffer-manager (get-game controller))))


(defmethod setup ((controller rlk--controller-game))
  "Initiates key binding on controller"
  (with-current-buffer (get-target-buffer (get-renderer controller))
    (setq rlk-controller controller)
    (dolist (binding (get-key-bindings controller))
      (local-set-key (car binding)
                     (intern (concat "rlk-command-"
                                          (symbol-name (cdr binding))))))))


(provide 'roguel-ike-controller)

;;; roguel-ike-controller.el ends here
