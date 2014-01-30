;;; file-system.el --- File system interactions

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
(require 'roguel-ike/hero-data)
(require 'roguel-ike/custom)

(defcustom rlk-save-directory (concat user-emacs-directory "roguel-ike-saves/")
  "The directory in which games are saved."
  :type 'directory
  :group 'roguel-ike
  :set (lambda (symbol value)
         (make-directory value t)))

(defun rlk--file-system-get-hero-filename (hero-name)
  "Return the file name correspondign to the save of HERO-NAME's data."
  (concat rlk-save-directory hero-name ".el"))

(defun rlk--file-system-save-hero (hero-data)
  "Save the HERO-DATA in the file system."
  (let ((hero-list (to-list hero-data))
        (file-name (rlk--file-system-get-hero-filename (get-name hero-data))))
    (with-temp-file file-name
      (prin1 hero-list (current-buffer)))))

(defun rlk--file-system-load-hero (hero-name)
  "Load the hero which name is HERO-NAME."
  (let ((file-name (rlk--file-system-get-hero-filename hero-name)))
    (if (file-exists-p file-name)
        (with-temp-buffer
          (insert-file-contents-literally file-name)
          (rlk--hero-data-create-from-list (read (current-buffer))))
      (error "No saved game with the name %s" hero-name))))

(provide 'roguel-ike/file-system)

;;; file-system.el ends here
