;;;; cl-gitlab.lisp

(in-package #:cl-gitlab)

(defvar *gitlab-credentials* nil)
(defvar *gitlab-status* nil)

(defun get-gitlab-token ()
  (with-open-file (stream ".gitlab.lisp" :if-does-not-exist nil)
     (with-standard-io-syntax
       (setf *gitlab-credentials* (read stream)))))

(get-gitlab-token)

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
  (ignore-errors
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
							       failed))))))))

(defun gitlab-get-failed-pipeline-project-names ()
  (mapcar (lambda (x) (format t "~a~%" x)) (getf (getf *gitlab-status* :failed) :projects)))

