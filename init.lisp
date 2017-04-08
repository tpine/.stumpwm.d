;;; Load extra packages
(ql:quickload :cl-utilities)

(in-package :stumpwm)

;;; Load Swank
(require :swank)
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)

(setq *startup-message* (format nil "Welcome Thomas!~%Swank is on port 4004~%Happy Hacking!"))

;; Startup Programs
(run-shell-command "feh --bg-scale /home/thomas/Pictures/wallpapers/outlast_wal_01.png")

;;; Set Time Input String
(setf *time-format-string-default* "%a %b %e %Y %l:%M%P")

;; set module directory (if not already set)
(set-module-dir "/home/thomas/.stumpwm.d/modules/")

;;; Load pavol changes
(load "~/.stumpwm.d/volume.lisp")
(load "~/.stumpwm.d/mode-line.lisp")
(load "~/.stumpwm.d/keybindings.lisp")

