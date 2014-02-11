;;; controller.el --- Controllers' code

;; Copyright (C) 2014 Steven Rémot

;;; Author: Steven Rémot

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Define roguel-ike controls system

;;; Code:
(require 'roguel-ike/game)
(require 'roguel-ike/graphics/renderer/game)
(require 'roguel-ike/graphics/renderer/stats)
(require 'roguel-ike/graphics/widget/entity)
(require 'roguel-ike/fov)
(require 'roguel-ike/physics/world)
(require 'popup)

(defvar-local rlk--local-controller nil
  "Game controller associated to the buffer.")

(defclass rlk--controller ()
  ((game :initarg :game
         :type rlk--game
         :reader get-game
         :protection :private
         :documentation "Game state.")
   (game-renderer :initarg :game-renderer
                  :type rlk--graphics-renderer-game
                  :reader get-game-renderer
                  :protection :private
                  :documentation "Level renderer.")
   (stats-renderer :initarg :stats-renderer
                   :type rlk--graphics-renderer-stats
                   :reader get-stats-renderer
                   :protection :private
                   :documentation "Statistics renderer.")
   (key-bindings :initform (("h" . move-left)
                            ("j" . move-down)
                            ("k" . move-up)
                            ("l" . move-right)
                            ("y" . move-left-up)
                            ("b" . move-left-down)
                            ("u" . move-right-up)
                            ("n" . move-right-down)
                            ("." . wait)
                            ("s" . select-and-use-skill)
                            ("c" . close-door)
                            (":" . examine-next-enemy)
                            ("q" . quit-rlk))
                 :type list
                 :reader get-key-bindings
                 :protection :private
                 :allocation :class
                 :documentation "Game controls.")
   (visible-enemies :initform ()
                    :type list
                    :protection :private
                    :documentation "List of all visible enemies not examinated yet.")
   (popup :protection :private
          :documentation "Interface popup."))
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
   (append (list 'defmethod name args docstring)
           (list '(hide-popup self))
           body)
   (list 'defun
         (intern (concat "rlk-command-" (symbol-name name)))
         '()
         docstring
         '(interactive)
         (list name 'rlk--local-controller))))

(defmethod get-hero ((self rlk--controller))
  "Return the hero in the game associated to the controller."
  (get-hero (get-game self)))

(defmethod get-hero-behaviour ((self rlk--controller))
  "Return the hero's behaviour for the game associated to the controller."
  (get-behaviour (get-hero self)))

(defmethod hide-popup ((self rlk--controller))
  "Hide the popup if there is any."
  (when (slot-boundp self 'popup)
    (popup-hide (oref self popup))
    (slot-makeunbound self 'popup)))

;; TODO try to displace fov elsewhere
(defmethod call-renderers ((self rlk--controller))
  "Ask the game-renderer to render game's level."
  (let* ((game (get-game self))
        (level (get-current-level game))
        (hero (get-hero game)))
    (rlk--fov-apply level hero)
    (oset self visible-enemies '())
    (setq buffer-read-only nil)
    (draw-level (get-game-renderer self) level)
    (draw-stats (get-stats-renderer self))
    (setq buffer-read-only t)))

(defvar rlk--direction-map
  '((move-left . (-1 . 0))
    (move-right . (1 . 0))
    (move-up . (0 . -1))
    (move-down . (0 . 1))
    (move-left-up . (-1 . -1))
    (move-left-down . (-1 . 1))
    (move-right-up . (1 . -1))
    (move-right-down . (1 . 1)))
  "Mapping between command names and direction.")

;; Direction commands definition
(dolist (direction-cons rlk--direction-map)
  (eval `(rlk--defcommand ,(car direction-cons) ((self rlk--controller))
                  "Move the hero."
                  (interact-with-cell (get-hero-behaviour self)
                                      ,(cadr direction-cons)
                                      ,(cddr direction-cons)))))

(rlk--defcommand wait ((self rlk--controller))
  "Wait one turn without doing anything."
  (wait (get-hero-behaviour self)))

(rlk--defcommand close-door ((self rlk--controller))
  "Ask the user for a direction, and try to close a door in this direction."
  (call-with-direction self (apply-partially 'close-door (get-hero-behaviour self))))

(rlk--defcommand select-and-use-skill ((self rlk--controller))
  "Ask the user for a skill and use it."
  (select-and-use-skill (get-hero-behaviour self)))

(rlk--defcommand examine-next-enemy ((self rlk--controller))
  "Examine the next visible enemy if there is one."
  (let ((visible-enemies (oref self visible-enemies)))
    (when (null visible-enemies)
      (setq visible-enemies (get-lit-entities (get-current-level (get-game self)))))

    (unless (null visible-enemies)
      (let ((entity (car visible-enemies)))
        (oset self visible-enemies (cdr visible-enemies))

        (if (is-hero-p entity)
            (examine-next-enemy self)
          (goto-char (point-min))
          (beginning-of-line (1+ (get-y entity)))
          (forward-char (get-x entity))
          (oset self popup (popup-tip (render (rlk--graphics-widget-entity "Entity widget"
                                                                           :entity entity
                                                                           :parts '(:name :effects :stats)))
                                      :point (point)
                                      :nowait t))
          (popup-draw (oref self popup)))))))

(rlk--defcommand quit-rlk ((self rlk--controller))
  "Quit roguel-ike."
  (kill-buffers (get-buffer-manager (get-game self))))

;;;;;;;;;;;;;;;;;;;
;; Input queries ;;
;;;;;;;;;;;;;;;;;;;

(defmethod call-with-direction ((self rlk--controller) function)
  "Ask the user for a direction and execute FUNCTION with the provided direction.

The direction takes the form of two arguments: dx and dy.

If the user did not enter a direction, FUNCTION is not executed, and
an error message is displayed."
  (let (direction)
    ;; Deferred assignation is required.
    ;; ask-direction is not effective otherwise.
    (setq direction  (ask-direction self))
    (if direction
        (funcall function (car direction) (cdr direction))
      (message "This is not a valid direction."))))

(defmethod ask-direction ((self rlk--controller))
  "Ask the user to input a direction.
If a direction is given, return a cons representing it.

The system will keep asking for a direction as long as
the input is invalid."
  (let* ((input (read-key-sequence "Enter a direction: "))
         (key (if (stringp input)
                  (substring input 0 1)
                (nth 0 input)))
         (command (cdr (assoc key (get-key-bindings self)))))
    (if command
        (let ((direction-cons (assoc command rlk--direction-map)))
          (if direction-cons
              (cdr direction-cons)
            (ask-direction self)))
      (ask-direction self))))

(defmethod ask-option ((self rlk--controller) prompt collection)
  "Ask the user to select an element of a collection.

PROMPT is the message displayed to invite the user to give an input.

COLLECTION is a list containing all the values the can be provided
by the user.

Return nil when the action has been cancelled."
  (let ((answer (completing-read prompt collection nil t)))
    (if (= 0 (length answer))
        nil
      answer)))

;;;;;;;;;;;;;;;;;;;;;;
;; Controller setup ;;
;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-keymap :static ((self rlk--controller))
  "Return a mode keymap according to the controller's key bindings."
  (let ((map (make-sparse-keymap)))
    (dolist (binding (oref-default self key-bindings))
      (define-key map (kbd (car binding)) (intern
                                           (concat "rlk-command-"
                                                   (symbol-name (cdr binding))))))
    map))

(provide 'roguel-ike/controller)
;;; controller.el ends here
