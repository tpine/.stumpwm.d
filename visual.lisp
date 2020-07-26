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


(apply-theme (gethash 'gruvbox *themes*))

(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Bold" :slant "r" :size 10))

(defvar *email-update-lock* (bt:make-lock))

(defun get-unread-emails ()
  (bt:make-thread
   (lambda ()
     (if (bt:acquire-lock *email-update-lock*)
	 (progn
	   (cl-gmail-oauth:get-unread-emails)
	   (bt:release-lock *email-update-lock*)))))
  
  (cond ((equal cl-gmail-oauth:*unread-emails* "0") "^[^2*EMAILS^]")
	((not cl-gmail-oauth:*unread-emails*) "^[^1*EMAILS^]")
	(t (format nil "^[^3*~a EMAILS^]" cl-gmail-oauth:*unread-emails*))))


(defun battery-format (ml)
  (declare (ignore ml))
  (let ((battery-line (battery-portable::fmt-bat nil)))
    (if (equal battery-line "(no battery)")
	""
	(concat battery-line " | "))))

(defvar *gitlab-update-lock* (bt:make-lock))

(defun get-gitlab-ci-message ()
  (bt:make-thread
   (lambda ()

       (if (bt:acquire-lock *gitlab-update-lock*)
	   (progn (cl-gitlab:get-pipeline-status)
		  (bt:release-lock *gitlab-update-lock*)))))

  (let ((failed-count (getf (getf cl-gitlab:*gitlab-status* :failed) :number)))
    (format t "~a" failed-count)
    (cond ((not cl-gitlab:*gitlab-status*) "^[^1*CI^]")
	  ((equal failed-count 0) "^[^2*CI^]")
	  ((> failed-count 0) (format nil "^[^1*~a CI^]" failed-count)))))

(defun get-utc-time ()
  (subseq (run-shell-command "date -u +%H:%M" t) 0 5))


;; Show time, cpu usage and network traffic in the modelinecomment 
(setf *screen-mode-line-format*
      (list '(:eval (battery-format)) '(:eval (time-format "%H:%M")) " EST | " '(:eval (get-utc-time)) " UTC | " '(:eval (get-unread-emails)) " | " '(:eval (get-gitlab-ci-message))  " | %W"))

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
