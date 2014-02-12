;;; effects.el --- Effects definition

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
(require 'roguel-ike/stats/effect)

(rlk--defeffect :type :poison
                :name "Poison"
                :start-message '(Me ("are" . "is") "poisonned.")
                :end-message '(Me ("are" . "is") "no longer poisonned.")
                :period 5
                :apply-number 10
                :stats-change '(:health -1)
                :minimal-values '(:health 1))

(rlk--defeffect :type :tough
                :name "Tough"
                :start-message '(Me ("feel" . "feels") "tough!")
                :period 1
                :apply-number 1
                :immediate t
                :stats-change '(:strength 10
                                          :constitution 10))

(rlk--defeffect :type :burning
                :name "Burning"
                :start-message '(Me ("are" . "is") "burning!")
                :end-message '(Me ("feel" . "feels") "cool now.")
                :period 5
                :apply-number 10
                :stats-change '(:health -1))

(rlk--defeffect :type :healing
                :name "Healing"
                :start-message '(Me ("feel" . "feels") "healthier.")
                :period 1
                :apply-number 15
                :stats-change '(:health 1))

(rlk--defeffect :type :supersonic
                :name "Supersonic"
                :start-message '(Me ("are" . "is") "moving faster.")
                :end-message '(Me ("are" . "is") "coming back to normal speed.")
                :period 1
                :apply-number 1
                :immediate t
                :stats-change '(:speed 20
                                :constitution -10))

(rlk--defeffect :type :spiritual
                :name "Spiritual"
                :start-message '(Me ("feel" . "feels") "abstract.")
                :period 1
                :apply-number 1
                :immediate t
                :stats-change '(:spirit 20
                                :constitution -10))

(provide 'roguel-ike/data/effects)
;;; effects.el ends here
