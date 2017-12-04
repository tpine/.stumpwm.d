(in-package :stumpwm)

;; Double check prefix-key is set correctly
(set-prefix-key (kbd "C-t"))

;; Emacs Style Frame Splitting
(define-key *root-map* (kbd "0") "remove")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")

(defmacro make-program-binding (program-name window-class keybinding &optional alias)
  "Create run-or-raise and run-or-pull commands for program-name
window-class is the windows-class
Also add keybinding to the commands. 
C-keybinding r calls run-or-raise
C-keybinding p calls run-or-pull
C-keybinding n creates a new instance of the program"
  (if (not alias)
      (setf alias program-name))
  `(progn
     (defvar ,(intern (format nil "*~a-map*" alias)) nil)

     (defcommand ,(intern (format nil "~a" alias)) () () (run-shell-command ,program-name))
     
     (defcommand ,(intern (format nil "run-or-raise-~a" alias)) () ()
		 (run-or-raise ,program-name '(:class ,window-class)))
     
     (defcommand ,(intern (format nil "run-or-pull-~a" alias)) () ()
		 (run-or-pull ,program-name '(:class ,window-class)))
     
     (fill-keymap ,(intern (format nil "*~a-map*" alias))
		  (kbd "p") ,(format nil "run-or-pull-~a" alias)
		  (kbd "r") ,(format nil "run-or-raise-~a" alias)
		  (kbd "n") ,(format nil "~a" alias))
     
     (defparameter ,(intern (format nil "*~a-keybind-fragment*" alias)) '(,keybinding ,(intern (format nil "*~a-map*" alias))))))

(defparameter *layout-map* (make-sparse-keymap))

(define-key *root-map* (kbd "l") *layout-map*)

(defmacro make-layout (name programs keybinding)
  "Make a keymap which conatins the settings for all programs
For programs defined using an alias use the alias instead of the program name"
  `(progn
     (defcommand ,(intern (format nil "switch-to-~a-layout" name)) () ()
       "Fill *root-map* with the first layout in the list of layouts and hang the layouts off (kbd l)"
       (loop for program in ',(loop for program in programs
				  collect (let ((fragment (symbol-value (intern (format nil "*~a-keybind-fragment*" program)))))
					    (list 'define-key '*root-map* (list 'kbd (first fragment)) (second fragment))))
	     do (eval program)))
     
     (define-key *layout-map* (kbd ,keybinding) ,(format nil "switch-to-~a-layout" name))))



(make-program-binding "firefox-developer" "Firefox" "f" "firefox")

(make-program-binding "thunar" "Thunar" "m")

(make-program-binding "terminator" "Terminator" "c")

(make-program-binding "emacsclient -c -a emacs" "Emacs" "e" "emacs")

(make-layout "home" ("emacs"
		     "firefox"
		     "thunar"
		     "terminator")
	     "h")

(run-commands "switch-to-home-layout")

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

