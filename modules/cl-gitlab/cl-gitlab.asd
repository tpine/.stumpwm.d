;;;; cl-gitlab.asd

(asdf:defsystem #:cl-gitlab
  :description "Module to get gitlab information"
  :author "Thomas Atkinson <tnatkinson@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :depends-on (:drakma
	       :yason
	       :flexi-streams
	       :alexandria)
  
  :serial t
  :components ((:file "package")
               (:file "cl-gitlab")))
