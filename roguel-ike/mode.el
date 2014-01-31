;;; mode.el --- Base major mode

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
;;

;;; Code:
(require 'roguel-ike/game-screen)

(defvar rlk--mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'rlk--mode-quit)
    map)
  "Base keymap for roguel-ike modes.")

(define-derived-mode rlk--mode special-mode "roguel-ike"
  "This mode is the base of all other roguel-ike modes."
  (setq buffer-read-only t))

(defun rlk--mode-quit ()
  "Quit the game."
  (interactive)
  (quit-game rlk--local-game-screen))

(provide 'roguel-ike/mode)

;;; mode.el ends here
