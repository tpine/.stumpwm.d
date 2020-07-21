;;;; package.lisp

(defpackage #:cl-gitlab
  (:use #:cl)
  (:export #:*gitlab-status*
	   #:get-pipeline-status))
