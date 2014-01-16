;;; roguel-ike-controller.el --- Controllers' code

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

(defmethod call-renderers ((controller rlk--controller-game))
  "Ask the renderer to render game's level."
  (draw-level (get-renderer controller)
             (get-current-level (get-game controller))))

(defmethod update-game ((controller rlk--controller-game))
  "Update enemies, and call renderers."
  (let* ((game (get-game controller))
         (level (get-current-level game))
         (hero (get-hero game)))
    (add-time-delay-enemies level (get-time-delay hero))
    (update-enemies level)
    (add-time-delay hero (get-time-delay hero))
    (call-renderers controller)))

(rlk--defcommand move-left ((controller rlk--controller-game))
  "Move the hero left."
  (interact-with-cell (get-hero controller) -1 0)
  (update-game controller))

(rlk--defcommand move-right ((controller rlk--controller-game))
  "Move the hero right."
  (interact-with-cell (get-hero controller) 1 0)
  (update-game controller))

(rlk--defcommand move-up ((controller rlk--controller-game))
  "Move the hero up."
  (interact-with-cell (get-hero controller) 0 -1)
  (update-game controller))

(rlk--defcommand move-down ((controller rlk--controller-game))
  "Move the hero down."
  (interact-with-cell (get-hero controller) 0 1)
  (update-game controller))

(rlk--defcommand move-left-up ((controller rlk--controller-game))
  "Move the hero left-up."
  (interact-with-cell (get-hero controller) -1 -1)
  (update-game controller))

(rlk--defcommand move-left-down ((controller rlk--controller-game))
  "Move the hero left-down."
  (interact-with-cell (get-hero controller) -1 1)
  (update-game controller))

(rlk--defcommand move-right-up ((controller rlk--controller-game))
  "Move the hero right-up."
  (interact-with-cell (get-hero controller) 1 -1)
  (update-game controller))

(rlk--defcommand move-right-down ((controller rlk--controller-game))
  "Move the hero right-down."
  (interact-with-cell (get-hero controller) 1 1)
  (update-game controller))

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
