;;; select-mode.el --- Mode selection screen

;;; Commentary:
;;

;;; Code:
(require 'cl-generic)
(require 'roguel-ike/mode/menu)
(require 'roguel-ike/game-screen/test)
(require 'roguel-ike/game-screen/arena)
(require 'roguel-ike/game-screen/dungeon-rampage)
(require 'roguel-ike/entity/factory/hero)
(require 'roguel-ike/graphics/widget/entity)
(require 'roguel-ike/graphics/faces)

(defclass rlk--game-screen-select-mode (rlk--game-screen)
  ((hero-data :type rlk--hero-data
              :reader get-hero-data
              :protection :private
              :documentation "Current hero data."))
  "Mode selection screen.")

(cl-defmethod setup ((self rlk--game-screen-select-mode) hero-data)
  "Save hero data and draw menu."
  (let* ((buffer-manager (get-buffer-manager self))
         (game-buffer (get-game-buffer buffer-manager)))
    (save-hero (get-hero-data-manager self) hero-data)
    (oset self hero-data hero-data)

    (setup-menu-layout buffer-manager)
    (set-buffer game-buffer)
    (draw-screen self)
    (rlk--menu-mode)
    (register-in-buffers self game-buffer)))

(cl-defmethod draw-screen ((self rlk--game-screen-select-mode))
  "Render the user interface on the current buffer."
  (let ((game-modes (list
                     (list
                      "Arena"
                      "Kill all your enemies in an arena"
                      (lambda (self button)
                        (call-end-callback self
                                           'rlk--game-screen-arena
                                           (get-hero-data self))))
                     (list
                      "Dungeon rampage"
                      "Go as far as possible in an infinite dungeon."
                      (lambda (self button)
                        (call-end-callback self
                                           'rlk--game-screen-dungeon-rampage
                                           (get-hero-data self))))
                     (list
                      "Test"
                      "A simple mode for testing purposes"
                      (lambda (self button)
                        (call-end-callback self
                                           'rlk--game-screen-test
                                           (get-hero-data self)))))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (propertize "Select a game mode:" 'face 'rlk-face-title))
    (insert "\n\n")

    (dolist (game-mode game-modes)
      (insert-text-button (car game-mode)
                          'action (apply-partially (nth 2 game-mode) self))
      (insert (concat "\n"
                      (nth 1 game-mode)
                      "\n\n")))

    (insert (propertize "Your character:" 'face 'rlk-face-title))
    (insert "\n\n")
    (insert (render (rlk--graphics-widget-entity
                     :entity (rlk--entity-create-from-hero-data (get-hero-data self))
                     :parts '(:stats :skills))))

    (setq buffer-read-only t)))

(provide 'roguel-ike/game-screen/select-mode)

;;; select-mode.el ends here
