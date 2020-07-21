(ql:quickload :bt-semaphore)
(ql:quickload :drakma)
(ql:quickload :cxml)
(ql:quickload :clss)
(ql:quickload :lquery)
(ql:quickload :plump)
(ql:quickload :xpath)
(ql:quickload :yason)

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

(defparameter *unread-emails* nil)
(defparameter *updating-email-count* nil)

(defvar *google-tokens* nil)


(defun load-google-credentials ()
  (with-open-file (stream ".google.lisp")
     (with-standard-io-syntax
       (setf *google-tokens* (read stream)))))

(defun save-google-credentials ()
  (with-open-file (out ".google.lisp"
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *google-tokens* out))))

(load-google-credentials)

(defun refresh-access-token ()
  (let ((refresh-token (getf *google-tokens* :refresh-token))
	(client-id (getf *google-tokens* :client-id))
	(client-secret (getf *google-tokens* :client-secret)))
    (setf (getf *google-tokens* :access-token)
	  (gethash
	   "access_token"
	   (yason:parse (drakma:http-request
			 "https://accounts.google.com/o/oauth2/token"
			 :method :post
			 :parameters (list (cons "client_id" client-id)
					   (cons "client_secret" client-secret)
					   (cons "refresh_token" refresh-token)
					   (cons "grant_type" "refresh_token"))
			 :want-stream t))))))


(defun get-unread-email-count (xml)
  (format t "~a" xml)
  (let ((doc (plump:parse xml)))
    (plump:render-text (car (plump:get-elements-by-tag-name doc "fullcount")))))

(defun get-unread-emails ()
  (bt:make-thread
   (lambda ()

       (if (equal *updating-email-count* nil)
	   (progn (setf *updating-email-count* t)
		  (handler-case
		      (let* ((request (multiple-value-list (drakma:http-request
						      "https://mail.google.com/mail/feed/atom"
						      :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" (getf *google-tokens* :access-token)))))))
			     (stream (nth 0 request)))
			(cond ((equal (nth 1 request) 401)
			       (refresh-access-token)
			       (setf *unread-emails* nil
				     *updating-email-count* nil))
			      (t
			       (setf *unread-emails* (get-unread-email-count stream)
				     *updating-email-count* nil))))
		    (usocket:ns-try-again-condition (c) ;; <-- optional argument
		      (declare (ignore c))
		      (setf *unread-emails* nil
			    *updating-email-count* nil)))))))
   
  (cond ((equal *unread-emails* "0") "^[^2*EMAILS^]")
	((not *unread-emails*) "^[^1*EMAILS^]")
	(t (format nil "^[^3*~a EMAILS^]" *unread-emails*))))

(defun battery-format (ml)
  (declare (ignore ml))
  (let ((battery-line (battery-portable::fmt-bat nil)))
    (if (equal battery-line "(no battery)")
	""
	(concat battery-line " | "))))

(defvar *gitlab-credentials* nil)
(defvar *gitlab-status* nil)
(defvar *updating-gitlab-status* nil)

(defun get-gitlab-token ()
  (with-open-file (stream ".gitlab.lisp")
     (with-standard-io-syntax
       (setf *gitlab-credentials* (read stream)))))

(get-gitlab-token)

(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

(defun print-hash-entrys (hash)
   (with-hash-table-iterator (my-iterator hash)
    (loop
      (multiple-value-bind (entry-p key value)
          (my-iterator)
        (if entry-p
            (print-hash-entry key value)
            (return))))))

(defun get-gitlab-groups ()
  (let ((stream (drakma:http-request "https://gitlab.com/api/v4/groups/"
				      :additional-headers (list (cons "PRIVATE-TOKEN" (getf *gitlab-credentials* :access-token)))
				      :want-stream t)))
    (yason:parse stream)))

(defun get-gitlab-groups-projects (group)
  (let ((stream (drakma:http-request (format nil "https://gitlab.com/api/v4/groups/~a/projects" group)
				      :additional-headers (list (cons "PRIVATE-TOKEN" (getf *gitlab-credentials* :access-token)))
				      :want-stream t)))
    (yason:parse stream)))

(defun get-gitlab-pipeline (pipeline)
  (let ((test (multiple-value-list (drakma:http-request (format nil "https://gitlab.com/api/v4/projects/~a/pipelines" (car pipeline))
							:additional-headers (list (cons "PRIVATE-TOKEN" (getf *gitlab-credentials* :access-token)))))))
    ;; Highest pipeline id is first
    (let ((newest-pipeline (first (yason:parse (flexi-streams:octets-to-string (nth 0 test))))))
      
      (if newest-pipeline
	  (list :status (gethash "status" newest-pipeline) :name (car (last pipeline)))))))

(defun get-pipeline-status ()
  (let ((status (mapcar
		 (lambda (x)
		   (let ((id (gethash "id" x)))
		     (if id
			 (get-gitlab-pipeline (list (gethash "id" x) (gethash "name" x))))))
		 (alexandria:flatten
		  (mapcar
		   (lambda (x)  
		     (get-gitlab-groups-projects (gethash "id" x)))
		   (alexandria:flatten (get-gitlab-groups)))))))
    (let ((success (remove-if-not (lambda (x) (format t "~a" x) (equal "success" (getf x :status))) status))
	  (failed (remove-if-not (lambda (x) (equal "failed" (getf x :status))) status)))
      (setf *gitlab-status* (list :success (list-length success)
				  :failed (list :number (list-length failed)
						:projects (mapcar
							   (lambda (y) (getf y :name))
							   failed)))))))

(defun gitlab-get-failed-pipeline-project-names ()
  (mapcar (lambda (x) (format t "~a~%" x)) (getf (getf *gitlab-status* :failed) :projects)))

(defun get-gitlab-ci-message ()
  (bt:make-thread
   (lambda ()

       (if (equal *updating-gitlab-status* nil)
	   (progn (setf *updating-gitlab-status* t)
		  (get-pipeline-status)
		  (setf *updating-gitlab-status* nil)))))

  (let ((failed-count (getf (getf *gitlab-status* :failed) :number)))
    (format t "~a" failed-count)
    (cond ((not *gitlab-status*) "^[^1*CI^]")
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
