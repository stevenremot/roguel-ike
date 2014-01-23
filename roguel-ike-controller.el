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
(require 'roguel-ike-fov)

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
                            ("q" . quit-rlk)
                            ("." . wait))
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

(defmethod get-hero ((self rlk--controller-game))
  "Return the hero in the game associated to the CONTROLLER."
  (get-hero (get-game self)))

;; TODO try to displace fov elsewhere
(defmethod call-renderers ((self rlk--controller-game))
  "Ask the renderer to render game's level."
  (let* ((game (get-game self))
        (level (get-current-level game))
        (hero (get-hero game)))
    (rlk--fov-apply level hero)
    (draw-level (get-renderer self) level)))

(rlk--defcommand move-left ((self rlk--controller-game))
  "Move the hero left."
  (interact-with-cell (get-behaviour (get-hero self)) -1 0))

(rlk--defcommand move-right ((self rlk--controller-game))
  "Move the hero right."
  (interact-with-cell (get-behaviour (get-hero self)) 1 0))

(rlk--defcommand move-up ((self rlk--controller-game))
  "Move the hero up."
  (interact-with-cell (get-behaviour (get-hero self)) 0 -1))

(rlk--defcommand move-down ((self rlk--controller-game))
  "Move the hero down."
  (interact-with-cell (get-behaviour (get-hero self)) 0 1))

(rlk--defcommand move-left-up ((self rlk--controller-game))
  "Move the hero left-up."
  (interact-with-cell (get-behaviour (get-hero self)) -1 -1))

(rlk--defcommand move-left-down ((self rlk--controller-game))
  "Move the hero left-down."
  (interact-with-cell (get-behaviour (get-hero self)) -1 1))

(rlk--defcommand move-right-up ((self rlk--controller-game))
  "Move the hero right-up."
  (interact-with-cell (get-behaviour (get-hero self)) 1 -1))

(rlk--defcommand move-right-down ((self rlk--controller-game))
  "Move the hero right-down."
  (interact-with-cell (get-behaviour (get-hero self)) 1 1))

(rlk--defcommand wait ((self rlk--controller-game))
  "Wait one turn without doing anything."
  (wait (get-behaviour (get-hero self))))

(rlk--defcommand quit-rlk ((self rlk--controller-game))
  "Quit roguel-ike."
  (kill-buffers (get-buffer-manager (get-game self))))


(defmethod setup ((self rlk--controller-game))
  "Initiates key binding on controller"
  (with-current-buffer (get-target-buffer (get-renderer self))
    (setq rlk-controller self)
    (dolist (binding (get-key-bindings self))
      (local-set-key (car binding)
                     (intern (concat "rlk-command-"
                                          (symbol-name (cdr binding))))))))


(provide 'roguel-ike-controller)
;;; roguel-ike-controller.el ends here
