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
(load-module "battery-portable")
(load-module "ttf-fonts")

(set-font "-*-dejavu sans mono-bold-r-*-*-12-*-*-*-*-*-*-*")

;; Show time, cpu usage and network traffic in the modelinecomment 
(setf *screen-mode-line-format*
      (list "%B | " '(:eval (time-format "%l:%M")) "  |%W"))

(setf *window-format* "%n %10c: %15t|")

;; Turn on the modeline
(toggle-mode-line (current-screen) (current-head))
