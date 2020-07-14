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

(make-program-binding "terminator" "Terminator")

(make-program-binding "emacsclient -c" "Emacs" "emacs")

(make-program-binding "insomnia" "Insomnia")

(make-program-binding "mongodb-compass" "mongodb-compass")

(make-program-binding "dbeaver" "DBeaver")

(make-program-binding "spotify" "Spotify")

(make-program-binding "keepassxc" "keepassxc")

(define-key *root-map* (kbd "f") |*firefox-map*|)
(define-key *root-map* (kbd "e") |*emacs-map*|)
(define-key *root-map* (kbd "m") |*thunar-map*|)
(define-key *root-map* (kbd "c") |*terminator-map*|)
(define-key *root-map* (kbd "q") |*spotify-map*|)
(define-key *root-map* (kbd "p") |*keepassxc-map*|)

(defcommand work-keybindings () ()
  (define-key *root-map* (kbd "i") |*insomnia-map*|)
  (define-key *root-map* (kbd "r") |*mongodb-compass-map*|)
  (define-key *root-map* (kbd "d") |*dbeaver-map*|))

(defun focus-current-frame-on-other-head (group)
  "Focus first frame on the next head."
  (let* ((remaining-heads (cdr (member (group-current-head group) (screen-heads (current-screen)))))
	 (other-head (if (null remaining-heads)
			 (first (screen-heads (current-screen)))
			 (car remaining-heads))))
    (focus-frame group (first (remove-if-not (lambda (frame)
					       (eql (frame-head group frame)
						    other-head))
					     (group-frames group))))))

(defcommand work-setup () ()
  "Configuration for Work Computer"
  (run-shell-command "xrandr --output HDMI1 --off --output DP1 --mode 1920x1080 --pos 1920x0 --rotate normal --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off" t)
  (focus-current-frame-on-other-head (current-group))
  (mode-line)
  (work-keybindings)
  (fnext))

;; Setup bindings for less common aplications which would be opened then closed
(defcommand screenshot () ()
  "Do we wanna Scrot? Yeah! We wanna Scrot!"
  (run-shell-command "cd /home/thomas/Pictures/screenshots/; scrot"))

(defcommand screenshot-name () ()
	    "Do we wanna Scrot? Yeah! We wanna Scrot!"
	    (run-shell-command (concat "cd /home/thomas/Pictures/screenshots/; scrot temp.png") t)
	    (let ((filename (read-one-line (current-screen) "Filename:")))
	      (run-shell-command (concat "cd /home/thomas/Pictures/screenshots/; mv ./temp.png ./" filename ".png"))))

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
    m))

(defparameter *power-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p") "shutdown")
    (define-key m (kbd "r") "reboot")
    m)) 

(defparameter *layout-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "w") "work-setup")
    (define-key m (kbd "s") *screenshot-map*)
    m) )

(defparameter *system-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") *screenshot-map*)
    (define-key m (kbd "l") *layout-map*)
    (define-key m (kbd "p") *power-map*)
    m))

(define-key *root-map* (kbd "s") *system-map*)

(defcommand user-switch-to-screen (screen-num) ((:number "Screen Number: "))
  (select-window-by-number (window-number (car (head-windows (current-group)
							     (nth screen-num (group-heads (current-group)))))))
  (group-wake-up (current-group)))

(define-key *root-map* (kbd "F1") "user-switch-to-screen 1")
(define-key *root-map* (kbd "F2") "user-switch-to-screen 2")
(define-key *root-map* (kbd "F3") "user-switch-to-screen 0")

