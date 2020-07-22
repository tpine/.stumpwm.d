(in-package :stumpwm-user)

(defun reload-init ()
  "Restart Slynk and reload source.
This is needed if Sly updates while StumpWM is running"
  (slynk:stop-server 4004)
  (loadrc)
  (slynk:create-server :port 4004
		     :dont-close t))
