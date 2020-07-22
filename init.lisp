(in-package :stumpwm)
;;; If quicklisp does not exist we need to load it
;;; This is useful for guix based systems
(push "/home/thomas/.stumpwm.d/" asdf:*central-registry*)

;; set module directory (if not already set)
(set-module-dir "/home/thomas/.stumpwm.d/modules/")

(asdf:operate 'asdf:load-op :stumpwm-init)
