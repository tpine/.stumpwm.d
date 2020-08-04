(in-package :stumpwm)

;; Double check prefix-key is set correctly
(set-prefix-key (kbd "C-t"))

;; Emacs Style Frame Splitting
(define-key *root-map* (kbd "0") "remove")
(define-key *root-map* (kbd "1") "only")
(define-key *root-map* (kbd "2") "vsplit")
(define-key *root-map* (kbd "3") "hsplit")

(defmacro make-program-binding (program-name window-class &optional alias)
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
		  (kbd "n") ,(format nil "~a" alias))))

(make-program-binding "firefox-developer-edition" "firefoxdeveloperedition" "firefox")

(make-program-binding "thunar" "Thunar")

(make-program-binding "slack" "Slack")

(make-program-binding "terminator" "Terminator")

(make-program-binding "emacsclient -c" "Emacs" "emacs")

(make-program-binding "insomnia" "Insomnia")

(make-program-binding "mongodb-compass" "mongodb-compass")

(make-program-binding "dbeaver" "DBeaver")

(make-program-binding "spotify" "Spotify")

(make-program-binding "keepassxc" "keepassxc")

(defparameter *program-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") |*firefox-map*|)
    (define-key m (kbd "s") |*slack-map*|)
    (define-key m (kbd "e") |*emacs-map*|)
    (define-key m (kbd "m") |*thunar-map*|)
    (define-key m (kbd "c") |*terminator-map*|)
    (define-key m (kbd "q") |*spotify-map*|)
    (define-key m (kbd "p") |*keepassxc-map*|)
    m))

(define-key *root-map* (kbd "p") *program-map*)

;; Setup bindings for less common aplications which would be opened then closed
(defcommand screenshot () ()
  "Do we wanna Scrot? Yeah! We wanna Scrot!"
  (run-shell-command "cd /home/thomas/Pictures/screenshots/; scrot"))

(defcommand screenshot-name () ()
	    "Do we wanna Scrot? Yeah! We wanna Scrot!"
  (let ((filename (read-one-line (current-screen) "Filename:")))
    (run-shell-command (concat "cd /home/thomas/Pictures/screenshots/; scrot " filename ".png") t)))

(defcommand screenshot-select () ()
  "Do we wanna Scrot? Yeah! We wanna Scrot!"
  (run-shell-command "cd /home/thomas/Pictures/screenshots/; scrot -s"))

(defcommand screenshot-name-select () ()
  "Do we wanna Scrot? Yeah! We wanna Scrot!"	    
  (let ((filename (read-one-line (current-screen) "Filename:")))
    (run-shell-command (concat "cd /home/thomas/Pictures/screenshots/; scrot -s " filename ".png") t)))

(defcommand volume-control () ()
	    "Start volume control"
	    (run-or-raise "pavucontrol" '(:class "Pavucontrol")))

;;; Shutdown and Reboot
(defcommand shutdown (confirm) ((:y-or-n "Confirm Shutdown "))
  "Ask for the user to confirm before shutting down."
	    (if confirm
		(run-shell-command "poweroff")))

(defcommand reboot (confirm) ((:y-or-n "Confirm Reboot "))
  "Ask for the user to confirm before rebooting."
	    (if confirm
		(run-shell-command "reboot")))

;;; System Command Keymap
(defparameter *screenshot-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "f") "screenshot")
    (define-key m (kbd "n") "screenshot-name")
    (define-key m (kbd "s") "screenshot-select")
    m))

(defparameter *power-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p") "shutdown")
    (define-key m (kbd "r") "reboot")
    m)) 

(defparameter *system-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") *screenshot-map*)
    (define-key m (kbd "p") *power-map*)
    (define-key m (kbd "v") "volume-control")
    m))

(define-key *root-map* (kbd "s") *system-map*)

(defcommand user-switch-to-screen (screen-num) ((:number "Screen Number: "))
  "Only works when there is a currently open window on the screen"
  (select-window-by-number (window-number (car (head-windows (current-group)
							     (nth screen-num (group-heads (current-group)))))))
  (group-wake-up (current-group)))

(define-key *root-map* (kbd "F1") "user-switch-to-screen 2")
(define-key *root-map* (kbd "F2") "user-switch-to-screen 1")
(define-key *root-map* (kbd "F3") "user-switch-to-screen 0")

