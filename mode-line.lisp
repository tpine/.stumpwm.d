(in-package :stumpwm)

;;; Load battery module
(load-module "battery-portable")
(load-module "ttf-fonts")

(set-font "-*-dejavu sans mono-bold-r-*-*-12-*-*-*-*-*-*-*")

;;; Set Colors to match spaceline
(setq *mode-line-foreground-color* "#5d4d7a"
      *mode-line-background-color* "#292b2e"
      *mode-line-border-color* "#5d4d7a")

;; Show time, cpu usage and network traffic in the modelinecomment 
(setf *screen-mode-line-format*
      (list "%B |" '(:eval (time-format "%l:%M")) " |%W"))

(setf *window-format* "%n %10c: %15t|")

;; Turn on the modeline
(toggle-mode-line (current-screen) (current-head))
