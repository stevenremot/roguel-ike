;;; roguel-ike.el --- Main file for roguel-ike

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

;; Description:

;; The goal of this project is to develop a coffee-break roguelike for Emacs.

;; The complete game will allow the player to create various characters
;; and to make them fulfill challenges like arena fight and short dungeon
;; explorations.

;; There isn't any level system.  Each stat can improve independently, by
;; doing related actions.  For example, sucessfull attacks will at term improve
;; character's strength.

;; Usage:

;; To load the file write this in your Emacs configuration:

;;     (add-to-list 'load-path "/path/to/roguel-ike/")
;;     (require 'roguel-ike)

;; Then, type `M-x roguel-ike`.

;; * use _y, u, h, j, k, l, b, n_ for movements
;; * use _._ to wait one turn
;; * use _s_ to use a skill
;; * use _q_ to quit

;; State of the project:

;; The project is in early stage of development.

;; This is an experiment :

;; * This is my first serious attempt to create a roguelike
;; * I'm using Emacs Lisp because I want to make it run on Emacs,
;;   but also to gain experience in Lisp programming and to see
;;   how a large Lisp program can be managed

;; Compatibilities:

;; It has been tested on Emacs 24.3, it should work under Emacs 23 too.
;; If not, feel free to open an issue.

;; License:

;; This program is released under GPLv3 license.

;; See _LICENSE_ for more details.


;;; Code:
(require 'roguel-ike/hero-data/manager)
(require 'roguel-ike/buffer-manager)
(require 'roguel-ike/game-screen/select-hero)

(require 'roguel-ike/data/skills)
(require 'roguel-ike/data/races)

(defun rlk--start-screen (hero-data-manager buffer-manager screen-symbol &rest args)
  "Start a new screen.

HERO-DATA-MANAGER is the game's saved data manager.
BUFFER-MANAGER is the game's buffer manager.
SCREEN-SYMBOL is the screen's class name.
ARGS are the arguments to transfer to screen setup."
  (let (screen)
    (when screen-symbol
      (setq screen (make-instance screen-symbol
                                  :hero-data-manager hero-data-manager
                                  :buffer-manager buffer-manager
                                  :end-callback (apply-partially 'rlk--start-screen hero-data-manager buffer-manager)))
      (apply 'setup screen args))))

;;;###autoload
(defun roguel-ike ()
  "Start a roguel-ike game."
  (interactive)
  (rlk--start-screen (rlk--hero-data-manager "Hero data manager")
                     (rlk--buffer-manager "Buffer manager")
                     'rlk--game-screen-select-hero))

(provide 'roguel-ike)

;;; roguel-ike.el ends here
