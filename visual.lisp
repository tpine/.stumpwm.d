(ql:quickload :bt-semaphore)

(in-package :stumpwm)

;;; Rewrite fmt-head-window-list so that we have window title seperators
(defun fmt-head-window-list (ml)
  "Using *window-format*, return a 1 line list of the windows, space seperated."
  (format nil "~{~a~^ | ~} |"
          (mapcar (lambda (w)
                    (let ((str (format-expand *window-formatters* *window-format* w)))
                      (if (eq w (current-window))
                          (fmt-highlight str)
                          str)))
                  (sort1 (head-windows (mode-line-current-group ml) (mode-line-head ml))
                         #'< :key #'window-number))))


;;; Visual
(defvar *themes* (make-hash-table))
(defun add-theme (name theme)
  (setf (gethash name *themes*) theme))

;;; Colors based off spacemacs-dark-theme for emacs
(defclass theme ()
    ((fg
      :initarg :fg
      :type string)
     (bg
      :initarg :bg
      :type string)

     (border
      :initarg :border
      :type string)

     (focus
      :initarg :focus
      :type string)
     (unfocus
      :initarg :unfocus
      :type string)

     (mode-line-fg
      :initarg :mode-line-fg
      :type string)
     (mode-line-bg
      :initarg :mode-line-bg
      :type string)
     (mode-line-border
      :initarg :mode-line-border
      :type string)

     (color-map-first
      :initarg :color-map-first
      :type string)
     (color-map-last
      :initarg :color-map-last
      :type string)))

(defun apply-theme (theme)
  (set-fg-color (slot-value theme 'fg))
  (set-bg-color (slot-value theme 'bg))
  (set-border-color (slot-value theme 'border))
  (set-focus-color (slot-value theme 'focus))
  (set-unfocus-color (slot-value theme 'unfocus))

  (setf *mode-line-foreground-color* (slot-value theme 'mode-line-fg)
	*mode-line-background-color* (slot-value theme 'mode-line-bg)
	*mode-line-border-color* (slot-value theme 'mode-line-border))
  
  (setf (car *colors*) (slot-value theme 'color-map-first)
	(car (last *colors*)) (slot-value theme 'color-map-last))
  (update-color-map (current-screen)))

(let ((grey "#292b2e")
      (purple "#5d4d7a"))
  (add-theme 'spacemacs
	     (make-instance 'theme
	      :fg purple
	      :bg grey
	      :border purple
	      :focus purple
	      :unfocus grey
	      :mode-line-fg purple
	      :mode-line-bg grey
	      :mode-line-border purple
	      :color-map-first grey
	      :color-map-last purple)))

(let ((fg "#ebdbb2")
      (bg "#282828")
      (border "#665c54"))
  (add-theme 'gruvbox
	     (make-instance 'theme
	      :fg fg
	      :bg bg
	      :border border
	      :focus fg
	      :unfocus bg
	      :mode-line-fg fg
	      :mode-line-bg bg
	      :mode-line-border border
	      :color-map-first bg
	      :color-map-last fg)))

(apply-theme (gethash 'gruvbox *themes*))

;;; Load battery module
(load-module "notify")

;; Set notification text color to yellow to make it obvious
(in-package :notify)
(defun show-notification (app icon summary body)
  "Show the notification using standard STUMPWM::MESSAGE function"
  (declare (ignore app icon))
  (stumpwm:message "^B^[^3*~A ~A^]" summary body))
;;; Start notification server
(notify-server-toggle)

;; (load-module :ttf-fonts)

(in-package :stumpwm)
(load-module :battery-portable)

(ql:quickload :clx-truetype)
(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Bold" :slant "r" :size 10))

(load-module "cl-gmail-oauth")
(defvar *updating-email-count* nil)



(defun get-unread-emails ()
  (bt:make-thread
   (lambda ()
     (if (not *updating-email-count*)
	 (progn
	   (setf *updating-email-count* t)
	   (cl-gmail-oauth:get-unread-emails)
	   (setf *updating-email-count* nil)))))
  
  (cond ((equal cl-gmail-oauth:*unread-emails* "0") "^[^2*EMAILS^]")
	((not cl-gmail-oauth:*unread-emails*) "^[^1*EMAILS^]")
	(t (format nil "^[^3*~a EMAILS^]" cl-gmail-oauth:*unread-emails*))))


(defun battery-format (ml)
  (declare (ignore ml))
  (let ((battery-line (battery-portable::fmt-bat nil)))
    (if (equal battery-line "(no battery)")
	""
	(concat battery-line " | "))))

(load-module "cl-gitlab")
(defvar *updating-gitlab-status* nil)

(defun get-gitlab-ci-message ()
  (bt:make-thread
   (lambda ()

       (if (equal *updating-gitlab-status* nil)
	   (progn (setf *updating-gitlab-status* t)
		  (cl-gitlab:get-pipeline-status)
		  (setf *updating-gitlab-status* nil)))))

  (let ((failed-count (getf (getf cl-gitlab:*gitlab-status* :failed) :number)))
    (format t "~a" failed-count)
    (cond ((not cl-gitlab:*gitlab-status*) "^[^1*CI^]")
	  ((equal failed-count 0) "^[^2*CI^]")
	  ((> failed-count 0) (format nil "^[^1*~a CI^]" failed-count)))))

(defun get-utc-time ()
  (subseq (run-shell-command "date -u +%H:%M" t) 0 5))


;; Show time, cpu usage and network traffic in the modelinecomment 
(setf *screen-mode-line-format*
      (list '(:eval (battery-format)) '(:eval (time-format "%H:%M")) " EST | " '(:eval (get-utc-time)) " UTC | " '(:eval (get-unread-emails)) " | " '(:eval (get-gitlab-ci-message)) " | %W"))

(setf *window-format* "%n %10c: %15t")

;;; When windows are desroyed window numbers are not synced
;;; 2kays <https://github.com/2kays> posted a solution on
;;; the TipsAndTricks section of the wiki
;;; This will repack window numbers every time a window is killed
(stumpwm:add-hook stumpwm:*destroy-window-hook*
                  #'(lambda (win) (stumpwm:repack-window-numbers)))

;; Turn on the modeline
(mapcar (lambda (head)
	  (toggle-mode-line (current-screen) head))
	(group-heads (current-group)))
