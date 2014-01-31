;;; races.el --- Races definition

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
;; All the races definition can be found here.

;;; Code:

(require 'roguel-ike/race)

(rlk--defrace :human
              "human"
              (list
               :health 10
               :stamina 10
               :strength 5
               :constitution 5
               :speed 5
               :spirit 5)
              (list
               :health 2
               :stamina 2
               :strength 1
               :constitution 1
               :speed 1
               :spirit 1)
              (list
               :punch))

(rlk--defrace :rat
              "rat"
              (list
               :health 5
               :stamina 1
               :strength 2
               :constitution 2
               :speed 7
               :spirit 0)
              (list
               :health 1
               :stamina 1
               :strength 1
               :constitution 1
               :speed 2
               :spirit 0)
              '())

(provide 'roguel-ike/data/races)

;;; races.el ends here
