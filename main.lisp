(defpackage #:niko
  (:use #:cl)
  (:import-from #:niko/app
                #:*app*)
  (:export #:start
           #:stop
           #:create-tables))
(in-package #:niko)

(defparameter *app-handler* nil)

(defun start (address port)
  (format t "Hi, I'm Niko!~%")
  (setf *app-handler*
        (clack:clackup *app*
                       :server :woo
                       :address address
                       :port port)))

(defun stop ()
  (format t "Good night...~%")
  (clack:stop *app-handler*)
  (setf *app-handler* nil))

(defun create-tables ()
  :or-migration-command)
