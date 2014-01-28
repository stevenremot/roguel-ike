;;; roguel-ike-skill.el --- SKill system

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
;; Entities can use skills that provide them special actions.

;; A skill has statistics requirements, and allow an entity
;; to do something special (super-attack, teleportation, ...)
;; spending some of its statistics.

;;; Code:

(require 'eieio)

(defclass rlk--skill ()
  ((name :initarg :name
         :type string
         :reader get-name
         :protection :private
         :documentation "The name of the skill.")
   (tags :initarg :tags
         :type list
         :reader get-tags
         :protection :private
         :documentation "A list of tags that classifies the skill.

Current tags:
- :directional : when it is present, it means the action also considers a direction.")
   (requirements :initarg :requirements
                 :type list
                 :reader get-requirements
                 :protection :private
                 :documentation "Association list specifying the stats requirements for the skill.")
   (spend :initarg :spend
          :type list
          :reader get-spend
          :protection :private
          :documentation "Associative list specifying how much of each statistic
is spent when the skill is used.")
   (action :initarg :action
           :type function
           :reader get-action
           :protection :private
           :documentation "Define the action performed when the skill is used.

The function must have the entity as argument.

The function must return t if the skill have been used succesfully,
nil otherwise.

If the directionnal tag is present, the action must take two additionnal
parameters dx and dy."))
  "Define a skill.")

(defmethod do-action ((self rlk--skill) &rest arguments)
  "Apply skill's action with ARGUMENTS."
  (apply (get-action self) arguments))

;;;;;;;;;;;;;;;;;;;;;;
;; Skill definition ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar rlk--skills '()
  "All the available skills.")

(defun rlk--defskill (id name tags requirements spend action)
  "Define a new skill.

It is accessible with the id ID.

NAME is the skill's name.

TAGS is a list defining the skill's tags.

REQUIREMENTS is an associative list defining the stats required to learn the
skill.

SPEND is an associative list defining how much of each stat will be spent
at each use of the skill.

ACTION is the code executed when the skill is used.  See the skill class
documentation for more information."
  (add-to-list 'rlk--skills (cons id (rlk--skill (concat "Skill: " name)
                                                 :name name
                                                 :tags tags
                                                 :requirements requirements
                                                 :spend spend
                                                 :action action))))

(defun rlk--skill-get-skill (id)
  "Return the skill with the id ID."
  (cdr (assoc id rlk--skills)))

(provide 'roguel-ike-skill)

;;; roguel-ike-skill.el ends here
