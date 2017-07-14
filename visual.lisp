(in-package :stumpwm)

;;; Visual
;;; Colors based off spacemacs-dark-theme for emacs
(let ((grey "#292b2e")
      (purple "#5d4d7a"))

  (set-fg-color purple)
  (set-bg-color grey)
  (set-border-color purple)
  (set-focus-color purple)
  (set-unfocus-color grey)

  (setf *mode-line-foreground-color* purple
	*mode-line-background-color* grey
	*mode-line-border-color* purple)
  
  (setf (car *colors*) grey
	(car (last *colors*)) purple)
  (update-color-map (current-screen)))

;;; Load battery module
(load-module "notify")

;;; Set notification text color to yellow to make it obvious
(in-package :notify)
(defun show-notification (app icon summary body)
  "Show the notification using standard STUMPWM::MESSAGE function"
  (declare (ignore app icon))
  (stumpwm:message "^B^[^3*~A ~A^]" summary body))
;;; Start notification server
(notify-server-toggle)
(load-module :battery-portable)
(load-module :ttf-fonts)

(in-package :stumpwm)

(set-font "-*-dejavu sans mono-bold-r-*-*-12-*-*-*-*-*-*-*")

;; Show time, cpu usage and network traffic in the modelinecomment 
(setf *screen-mode-line-format*
      (list "%B | " '(:eval (time-format "%l:%M")) "  |%W"))

(setf *window-format* "%n %10c: %15t|")

;;; When windows are desroyed window numbers are not synced
;;; 2kays <https://github.com/2kays> posted a solution on
;;; the TipsAndTricks section of the wiki
;;; This will repack window numbers every time a window is killed
(stumpwm:add-hook stumpwm:*destroy-window-hook*
                  #'(lambda (win) (stumpwm:repack-window-numbers)))

;; Turn on the modeline
(toggle-mode-line (current-screen) (current-head))
