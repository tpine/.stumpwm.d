#!/usr/bin/emacs --script

; Return path to sly in .emacs.d/elpa
(package-initialize)
(require 'pkg-info)
(princ (pkg-info-package-version "sly"))

(provide 'slynk-path-fix)
;;; slynk-path-fix.sh ends here
