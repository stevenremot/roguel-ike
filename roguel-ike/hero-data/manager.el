;;; manager.el --- Saved hero data management

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

(defcustom roguel-ike-save-directory (concat user-emacs-directory "roguel-ike-saves/")
  "The directory in which games are saved."
  :type 'directory
  :group 'roguel-ike
  :set (lambda (symbol value)
         (make-directory value t)
         (setq roguel-ike-save-directory value)))

(defclass rlk--hero-data-manager ()
  ((save-directory :initarg :save-directory
                   :type string
                   :reader get-save-directory
                   :protection :private
                   :documentation "The directory in which hero data are saved."))
  "Handle persistent storage of hero data.")

(defmethod initialize-instance :after ((self rlk--hero-data-manager) slots)
  "Initialize base slots."
  (unless (slot-boundp self 'save-directory)
    (oset self save-directory roguel-ike-save-directory)))

(defmethod get-hero-filename ((self rlk--hero-data-manager) hero-name)
  "Return the file name correspondign to the save of HERO-NAME's data."
  (concat (get-save-directory self) hero-name ".el"))

(defmethod save-hero ((self rlk--hero-data-manager) hero-data)
  "Save the HERO-DATA in the file system."
  (let ((hero-list (to-list hero-data))
        (file-name (get-hero-filename self (get-name hero-data))))
    (with-temp-file file-name
      (prin1 hero-list (current-buffer)))))

(defmethod load-hero ((self rlk--hero-data-manager) hero-name)
  "Load the hero which name is HERO-NAME.

Raise an error when this hero does not exist."
  (let ((file-name (get-hero-filename self hero-name)))
    (if (file-exists-p file-name)
        (with-temp-buffer
          (insert-file-contents-literally file-name)
          (rlk--hero-data-create-from-list (read (current-buffer))))
      (error "No saved game with the name %s" hero-name))))

(defmethod get-saved-heros ((self rlk--hero-data-manager))
  "Return the name of all the saved heros."
  (let ((save-files (directory-files (get-save-directory self)
                                     nil ".el$")))
    (mapcar (lambda (save-file)
              (substring save-file 0 (- (length save-file) 3)))
            save-files)))

(provide 'roguel-ike/hero-data/manager)

;;; manager.el ends here
