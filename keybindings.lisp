(in-package :stumpwm)

;; Double check prefix-key is set correctly
(set-prefix-key (kbd "C-t"))

;; Emacs Style Frame Splitting
(define-key *root-map* (kbd "0") "remove")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")

(defmacro make-program-binding (program-name window-class keybinding)
  "Create run-or-raise and run-or-pull commands for program-name
window-class is the windows-class
Also add keybinding to the commands. 
C-keybinding r calls run-or-raise
C-keybinding p calls run-or-pull
C-keybinding n creates a new instance of the program"
  
  `(progn
     (defvar ,(intern (format nil "*~a-map*" program-name)) nil)

     (defcommand ,(intern (format nil "~a" program-name)) () () (run-shell-command ,program-name))
     
     (defcommand ,(intern (format nil "run-or-raise-~a" program-name)) () ()
		 (run-or-raise ,program-name '(:class ,window-class)))
     
     (defcommand ,(intern (format nil "run-or-pull-~a" program-name)) () ()
		 (run-or-pull ,program-name '(:class ,window-class)))
     
     (fill-keymap ,(intern (format nil "*~a-map*" program-name))
		  (kbd "p") ,(format nil "run-or-pull-~a" program-name)
		  (kbd "r") ,(format nil "run-or-raise-~a" program-name)
		  (kbd "n") ,(format nil "~a" program-name))
     
     (define-key *root-map* (kbd ,keybinding) (intern ,(format nil "*~a-map*" program-name)))))

(make-program-binding "firefox" "Firefox" "f")

(make-program-binding "thunar" "Thunar" "m")

(make-program-binding "emacs" "Emacs" "e")

;; Setup bindings for less common aplications which would be opened then closed
(defcommand screenshot () ()
	    "Do we wanna Scrot? Yeah! We wanna Scrot!"
	    (run-shell-command "cd /home/thomas/Pictures/screenshots/; scrot"))
(define-key *root-map* (kbd "s") "screenshot")

(defcommand screenshot-name () ()
	    "Do we wanna Scrot? Yeah! We wanna Scrot!"
	    (run-shell-command (concat "cd /home/thomas/Pictures/screenshots/; scrot temp.png") t)
	    (let ((filename (read-one-line (current-screen) "Filename:")))
	      (run-shell-command (concat "cd /home/thomas/Pictures/screenshots/; mv ./temp.png ./" filename ".png"))))
(define-key *root-map* (kbd "M-s") "screenshot-name")

(defcommand volume-control () ()
	    "Start volume control"
	    (run-or-raise "pavucontrol" '(:class "Pavucontrol")))
(define-key *root-map* (kbd "v") "volume-control")

;;; Shutdown and Reboot
(defcommand shutdown (confirm) ((:y-or-n "Confirm Shutdown "))
  "Ask for the user to confirm before shutting down."
	    (if confirm
		(run-shell-command "poweroff")))

(defcommand reboot (confirm) ((:y-or-n "Confirm Reboot "))
  "Ask for the user to confirm before rebooting."
	    (if confirm
		(run-shell-command "reboot")))

(define-key *root-map* (kbd "p") "shutdown")
(define-key *root-map* (kbd "C-p") "reboot")

