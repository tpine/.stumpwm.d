;; Set notification text color to yellow to make it obvious
(in-package :notify)
(defun show-notification (app icon summary body)
  "Show the notification using standard STUMPWM::MESSAGE function"
  (declare (ignore app icon))
  (stumpwm:message "^B^[^3*~A ~A^]" summary body))

;;; Start notification server
(notify-server-toggle)

