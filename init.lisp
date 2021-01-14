;;; Swap Caps with Ctrl
(run-shell-command "~/fix-screens.sh")
(run-shell-command "setxkbmap -option ctrl:swapcaps")
(setq *startup-message* (format nil "Welcome Thomas!~%Slynk is on port 4004~%Happy Hacking!"))

(load "~/.stumpwm.d/visual.lisp")
(load "~/.stumpwm.d/keybindings.lisp")

;; JAVA has some issues with non reparenting window managers
;; While we could set _JAVA_AWT_WM_NONREPARENTING=1 lets just say we are oracles tiling window manager
;; I find that funnier
(run-shell-command "wmname LG3D")
