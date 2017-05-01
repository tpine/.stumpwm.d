;;; Load extra packages
(ql:quickload :cl-utilities)

;;; Fixup slynk load path
(load "~/.stumpwm.d/slynk-fix.lisp")
(push (concatenate 'string (get-sly-path) "slynk/") asdf:*central-registry*)
(ql:quickload :slynk)

(in-package :stumpwm)

;;; Load Slynk
(slynk:create-server :port 4004
		     :dont-close t)

(setq *startup-message* (format nil "Welcome Thomas!~%Slynk is on port 4004~%Happy Hacking!"))

;;; Startup Programs
;;; Set Background
(run-shell-command "feh --bg-scale /home/thomas/Pictures/wallpapers/outlast_wal_01.png")
;;; Start redshift
(run-shell-command "redshift")


;;; Set Time Input String
(setf *time-format-string-default* "%a %b %e %Y %l:%M%P")

;;; Visual
;;; Colors based off spacemacs-dark-theme for emacs
(let ((grey "#292b2e")
      (purple "#5d4d7a"))

  (set-fg-color purple)
  (set-bg-color grey)
  (set-border-color purple)
  (set-focus-color purple)
  (set-unfocus-color grey)
  
  (setf (car *colors*) grey
	(car (last *colors*)) purple)
  (update-color-map (current-screen)))


;; set module directory (if not already set)
(set-module-dir "/home/thomas/.stumpwm.d/modules/")

;;; Load pavol changes
(load "~/.stumpwm.d/volume.lisp")
(load "~/.stumpwm.d/mode-line.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
