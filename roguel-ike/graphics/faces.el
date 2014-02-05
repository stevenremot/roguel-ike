;;; faces.el --- roguel-ike faces

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
;; In charge of roguel-ike rendering

;;; Commentary:
;; Define the custom faces for roguel-ike.

;;; Code:
(require 'roguel-ike/custom)

(defgroup roguel-ike-faces
  '()
  "Group for rlk faces"
  :group 'roguel-ike)

(defface rlk-face-default
  '()
  "Standard roguel-ike face"
  :group 'rlk-faces)

(defface rlk-face-title
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :height 240))
  "Menu title face"
  :group 'roguel-ike-faces)

(defface rlk-face-wall
  '((((class color) (min-colors 88))
      :inherit 'rlk-face-default))
  "Wall face"
  :group 'roguel-ike-faces)

(defface rlk-face-ground
  '((((class color) (min-colors 88))
      :inherit 'rlk-face-default
      :foreground "dim gray"))
  "Ground face"
  :group 'roguel-ike-faces)

(defface rlk-face-door
  '((((class color) (min-colors 88))
     :inherit 'rlk-face-default
     :foreground "sandy brown"))
  "Door face"
  :group 'roguel-ike-faces)

(defface rlk-face-shadow
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "gray25"))
  "Face for visited objects the player cannot see now."
  :group 'roguel-ike-faces)

(defface rlk-face-hero
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "yellow3"))
  "Hero face"
  :group 'roguel-ike-faces)


(defface rlk-face-human
  '((((class color) (min-colors 8))
    :inherit 'rlk-face-default
    :foreground "purple"))
  "Human face"
  :group 'roguel-ike-faces)

(defface rlk-face-rat
  '((((class color) (min-colors 8))
    :inherit 'rlk-face-default
    :foreground "red"))
  "Rat face"
  :group 'roguel-ike-faces)

(defface rlk-face-troll
  '((((class color) (min-colors 8))
    :inherit 'rlk-face-default
    :foreground "green"))
  "Troll face"
  :group 'roguel-ike-faces)

(defface rlk-face-good-stat
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "green"))
  "Good statistic face"
  :group 'roguel-ike-faces)

(defface rlk-face-average-stat
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "yellow"))
  "Average statistic face"
  :group 'roguel-ike-faces)

(defface rlk-face-bad-stat
  '((((class color) (min-colors 8))
     :inherit 'rlk-face-default
     :foreground "red"))
  "Bad statistic face"
  :group 'roguel-ike-faces)

(provide 'roguel-ike/graphics/faces)

;;; faces.el ends here
