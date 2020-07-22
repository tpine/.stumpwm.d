(asdf:defsystem #:stumpwm-init
  :description "Stumpwm Init System"
  :author "Thomas Atkinson <tnatkinson@gmail.com>"
  :license  "GPLv2"
  :version "0.0.1"
  :depends-on (:cl-utilities
	       :slynk
	       :stumpwm
	       :trivial-shell
	       :bt-semaphore
	       :clx-truetype

	       ;; Stumpwm Modules
	       :notify
	       :battery-portable
	       :ttf-fonts

	       ;; Submodules
	       :cl-gmail-oauth
	       :cl-gitlab)
  
  :serial t
  :components ((:file "utility")
	       (:file "notify")
	       (:file "theme")
	       (:file "visual")
	       (:file "keybindings")
               (:file "stumpwm-init")))
