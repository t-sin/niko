(defpackage #:niko
  (:use #:cl)
  (:export #:start
           #:stop
           #:create-tables))
(in-package #:niko)

(defun start (address port)
  (format t "Hi, I'm Niko!~%"))

(defun stop ()
  (format t "Good night...~%"))

(defun create-tables ()
  :or-migration-command)
