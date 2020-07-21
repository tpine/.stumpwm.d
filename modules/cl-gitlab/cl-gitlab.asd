;;;; cl-gitlab.asd

(asdf:defsystem #:cl-gitlab
  :description "Describe cl-gitlab here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:drakma
	       :yason
	       :flexi-streams
	       :alexandria)
  
  :serial t
  :components ((:file "package")
               (:file "cl-gitlab")))
