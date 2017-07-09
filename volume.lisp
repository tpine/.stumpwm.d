;;; Setup pavol

;;; Load package
(in-package :stumpwm)

(load-module :pavol)

;;; Changes to pavol to get volume to 150%
;;; This follows the maximum volume allowed by pavolcontrol
;;; This is to hacky to merge
(in-package :pavol)

(defparameter *max-volume* (if t
			       150
			       100)
  "The maximum allowed volume.
pavucontrol allows a max volume percentage of 150.
this may damage the speakers so make it be explicatly set.
The default value is 100.")

(defmethod (setf volume) (percentage (sink sink))
  (assert (<= 0 percentage *max-volume*))
  (pacmd "set-sink-volume ~a ~a"
         (sink-index sink)
         (percentage->integer percentage))
  percentage)

(defun volume-up (sink percentage)
  (setf (volume sink) (min (+ (volume sink) percentage) *max-volume*)))

(defun make-volume-bar (percent)
  "Return a string that represents a volume bar"
  (format nil "^B~3d%^b [^[^7*~a^]]"
          percent (stumpwm::bar (min 100 percent) 50 #\# #\:)))

;;; Setup keybindings
(in-package :stumpwm)

(define-key *top-map*
    (kbd "XF86AudioRaiseVolume")
  "pavol-vol+")

(define-key *top-map*
    (kbd "XF86AudioLowerVolume")
  "pavol-vol-")

(define-key *top-map*
    (kbd "XF86AudioMute")
  "pavol-toggle-mute")
