;;;; package.lisp

(defpackage #:cl-gmail-oauth
  (:use #:cl)
  (:export #:*unread-emails*
	   #:get-unread-emails))
