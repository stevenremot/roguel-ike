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
(require 'roguel-ike/game-screen)
(require 'roguel-ike/hero-data/manager)

(defclass rlk--game-screen-select-hero (rlk--game-screen)
  ((hero-data-manager :type rlk--hero-data-manager
                      :reader get-hero-data-manager
                      :protection :private
                      :documentation "The hero data manager to use."))
  "The game screen for selecting a saved hero.

It also allows to go to the hero creation screen.")

(defmethod setup ((self rlk--game-screen-select-hero) &optional hero-data-manager)
  "Register HERO-DATA-MANAGER, and display the selection screen."
  (let ((buffer-manager (get-buffer-manager self)))
    (unless hero-data-manager
      (setq hero-data-manager (rlk--hero-data-manager "Hero data manager")))
    (oset self hero-data-manager hero-data-manager)

    (setup-menu-layout buffer-manager)
    (with-current-buffer (get-game-buffer buffer-manager)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Select your hero :\n---\n")
      (dolist (hero (get-saved-heros hero-data-manager))
        (insert-text-button hero
                            'action (apply-partially 'select-hero self))
        (insert "\n"))
      (insert "---\n")
      (insert-text-button "Create a new hero"
                          'action (apply-partially 'create-new-hero self))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (forward-button 1))))

(defmethod select-hero ((self rlk--game-screen-select-hero) hero-name)
  "End the screen by loading and selecting the hero with HERO-NAME."
  (when (markerp hero-name)
    (setq hero-name (button-label hero-name)))
  (let* ((hero-data-manager (get-hero-data-manager self))
         (hero-data (load-hero hero-data-manager hero-name)))
    (call-end-callback self
                       'rlk--game-screen-test
                       hero-data-manager
                       hero-data)))

(defmethod create-new-hero ((self rlk--game-screen-select-hero &rest args))
  "End the screen by asking hero creation."
  (call-end-callback self
                     nil
                     (get-hero-data-manager self)))

(provide 'roguel-ike/game-screen/select-hero)

;;; select-hero.el ends here
