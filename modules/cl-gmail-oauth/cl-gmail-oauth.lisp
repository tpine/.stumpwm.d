;;;; cl-gmail-oauth.lisp

(in-package #:cl-gmail-oauth)

(defparameter *unread-emails* nil)
(defparameter *gmail-sync-errors* nil)

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
  (handler-case
      (let* ((request (multiple-value-list (drakma:http-request
					    "https://mail.google.com/mail/feed/atom"
					    :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" (getf *google-tokens* :access-token)))))))
	     (stream (nth 0 request)))
	(cond ((equal (nth 1 request) 401)
	       (refresh-access-token)
	       (setf *unread-emails* nil))
	      (t
	       (setf *unread-emails* (get-unread-email-count stream)))))
    (error (c) ;; <-- optional argument
      (setf *unread-emails* nil
	    *gmail-sync-errors* c))))
