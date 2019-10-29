;;; If quicklisp does not exist we need to load it
;;; This is useful for guix based systems
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

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
(run-shell-command "syncthing -no-browser -home=/home/thomas/.config/syncthing")
(run-shell-command "emacs --daemon")

;;; Swap Caps with Ctrl
(run-shell-command "setxkbmap -option ctrl:swapcaps")

;;; Set Time Input String
(setf *time-format-string-default* "%a %b %e %Y %l:%M%P")

;; JAVA has some issues with non reparenting window managers
;; While we could set _JAVA_AWT_WM_NONREPARENTING=1 lets just say we are oracles tiling window manager
;; I find that funnier
(run-shell-command "wmname LG3D")

;; set module directory (if not already set)
(set-module-dir "/home/thomas/.stumpwm.d/modules/")

(defun restart-slynk ()
  "Restart Slynk and reload source.
This is needed if Sly updates while StumpWM is running"
  (slynk:stop-server 4004)
  (push (concatenate 'string (get-sly-path) "slynk/") asdf:*central-registry*)
  (ql:quickload :slynk)
  (slynk:create-server :port 4004
		     :dont-close t))

;;; Load pavol changes
(load "~/.stumpwm.d/volume.lisp")
(load "~/.stumpwm.d/visual.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
