(in-package :stumpwm)

(setq *startup-message* (format nil "Welcome Thomas!~%Slynk is on port 4004~%Happy Hacking!"))

;;; Startup Programs
;;; Set Background
(run-shell-command "shepherd")
(run-shell-command "emacs --daemon")

;;; Set Time Input String
(setf *time-format-string-default* "%a %b %e %Y %l:%M%P")

;; JAVA has some issues with non reparenting window managers
;; While we could set _JAVA_AWT_WM_NONREPARENTING=1 lets just say we are oracles tiling window manager
;; I find that funnier
(run-shell-command "wmname LG3D")

;;; Load Slynk
(slynk:create-server :port 4004
		     :dont-close t)
