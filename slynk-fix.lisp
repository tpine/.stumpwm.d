(ql:quickload :trivial-shell)

(defvar *elpa-path* "/home/thomas/.emacs.d/elpa/")

(defun get-sly-path ()
  "Run script to get sly version number and append it to elpa path"
  (let ((sly-version "sly-"))
    (multiple-value-bind (out err proc) (trivial-shell:shell-command "/home/thomas/.stumpwm.d/slynk-path-fix.sh")
	(declare (ignore err proc))	
      (with-input-from-string (ver (remove #\newline (remove #\) (remove #\( out))))
	(setf sly-version (concatenate 'string sly-version (write-to-string (read ver)) "." (write-to-string (read ver)))))

      (concatenate 'string *elpa-path* sly-version "/"))))
