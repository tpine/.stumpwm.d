;;;; cl-gmail-oauth.asd
(asdf:defsystem #:cl-gmail-oauth
  :description "Describe cl-gmail-oauth here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:drakma
	       :plump
	       :xpath
	       :yason)
  :serial t
  :components ((:file "package")
               (:file "cl-gmail-oauth")))
