(ql:quickload :trivial-shell)

(defvar *elpa-path* "/home/thomas/.emacs.d/elpa/")

(defmacro run-emacs-script (&rest body)
  "Run body as emacs script"
  (let ((script nil))
    (loop for sexep in body
	  do (setf script (concatenate 'string script (string-downcase (format nil "~a" sexep)))))
    `(multiple-value-bind (out err proc) (trivial-shell:shell-command (format nil "emacs --batch --eval=\"(progn ~a)\"" ,script))
       (declare (ignore proc))
       (list :output (read-from-string out) :error err))))

(defun get-sly-path ()
  "Run script to get sly version number and append it to elpa path
We need to do this as elpa stores packages in the format <pkg-name>-<date>.<version>
This could probably be done using regex in the future"
  (let ((version (getf (run-emacs-script
			(package-initialize)
			(require 'pkg-info)
			(princ (pkg-info-package-version 'sly))) :output)))

    (concatenate 'string *elpa-path* "sly-" (write-to-string (first version)) "." (write-to-string (second version)) "/")))
