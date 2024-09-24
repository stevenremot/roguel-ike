;;; select-hero.el --- Hero selection screen

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
(require 'cl-generic)
(require 'roguel-ike/mode/menu)
(require 'roguel-ike/hero-data/manager)
(require 'roguel-ike/race)
(require 'roguel-ike/game-screen/select-mode)
(require 'roguel-ike/graphics/faces)

(defvar rlk--races nil)

(defclass rlk--game-screen-select-hero (rlk--game-screen)
  ()
  "The game screen for selecting a saved hero.

It also allows to go to the hero creation screen.")

(cl-defmethod setup ((self rlk--game-screen-select-hero))
  "Display the selection screen."
  (let ((buffer-manager (get-buffer-manager self)))
    (setup-menu-layout buffer-manager)
    (set-buffer (get-game-buffer buffer-manager))
    (draw-screen self)
    (rlk--select-hero-mode)
    (register-in-buffers self (get-game-buffer buffer-manager))))

(cl-defmethod draw-screen ((self rlk--game-screen-select-hero))
  "Render the user interface on the current buffer."
  (let ((hero-data-manager (get-hero-data-manager self)))
    (setq buffer-read-only nil)
    (erase-buffer)

    (insert (propertize "Select your hero :" 'face 'rlk-face-title))
    (insert "\n\n")

    (dolist (hero (get-saved-heros hero-data-manager))
      (insert-text-button hero
                          'action (apply-partially 'select-hero self))
      (insert "\n"))
    (insert "\n--------\n")
    (insert-text-button "Create a new hero"
                        'action (apply-partially 'create-hero self))
    (setq buffer-read-only t)))

(cl-defmethod select-hero ((self rlk--game-screen-select-hero) hero-name)
  "End the screen by loading and selecting the hero with HERO-NAME."
  (when (markerp hero-name)
    (setq hero-name (button-label hero-name)))
  (let* ((hero-data-manager (get-hero-data-manager self))
         (hero-data (load-hero hero-data-manager hero-name)))
    (call-end-callback self
                       'rlk--game-screen-select-mode
                       hero-data)))

(cl-defmethod create-hero ((self rlk--game-screen-select-hero) &optional button)
  "Ask the user information to create a new hero and start the game with it."
  (let ((hero-name "")
        (race-name "")
        (race-names (mapcar (lambda (race)
                              (get-name race))
                            rlk--races))
        (race nil)
        (hero-data nil))

    (while (= (length hero-name) 0)
      (setq hero-name (read-string "Hero's name: "))
      (when (= (length hero-name) 0)
        (message "You must set a name.")))

    (while (= (length race-name) 0)
      (setq race-name (completing-read "Hero's race: " race-names nil t))
      (when (= (length race-name) 0)
        (message "You must set a race.")))

    (dolist (available-race rlk--races)
      (when (equal race-name (get-name available-race))
        (setq race available-race)))

    (setq hero-data (rlk--hero-data
                     :name hero-name
                     :race (get-type race)
                     :stats (get-base-stats race)))

    (save-hero (get-hero-data-manager self) hero-data)
    (select-hero self hero-name)))

(cl-defmethod delete-hero ((self rlk--game-screen-select-hero) hero-name)
  "Ask the user for confirmation before deleting HERO-NAME."
  (when (yes-or-no-p (format "Are you sure you want to delete %s? "
                             hero-name))
    (delete-hero (get-hero-data-manager self) hero-name)
    (setq buffer-read-only nil)
    (beginning-of-line)
    (kill-line 1)
    (setq buffer-read-only t)))

;;;;;;;;;;;;;;;;;;;;;
;; Associated mode ;;
;;;;;;;;;;;;;;;;;;;;;

(defvar rlk--select-hero-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") 'rlk--select-hero-mode-delete)
    map)
  "Keymap for hero selection mode.")


(define-derived-mode rlk--select-hero-mode rlk--menu-mode "roguel-ike:select-hero"
  "This mode defines key bindings for the hero selection screen.")

(defun rlk--select-hero-mode-delete ()
  "Delete the hero on which the cursor is pointing at."
  (interactive)
  (let ((hero-name (button-label (point))))
    (when (not (equal hero-name ""))
      (delete-hero rlk--local-game-screen hero-name))))

(provide 'roguel-ike/game-screen/select-hero)

;;; select-hero.el ends here
