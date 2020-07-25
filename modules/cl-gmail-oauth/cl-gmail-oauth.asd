;;;; cl-gmail-oauth.asd
(asdf:defsystem #:cl-gmail-oauth
  :description "Module to get number of unread emails from gmail"
  :author "Thomas Atkinson <tnatkinson95@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :depends-on (:dexador
	       :plump
	       :xpath
	       :yason)
  :serial t
  :components ((:file "package")
               (:file "cl-gmail-oauth")))
