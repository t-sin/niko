(defpackage #:niko
  (:use #:cl)
  (:import-from #:niko/app
                #:*app*)
  (:import-from #:niko/views/main)
  (:export #:start
           #:stop
           #:create-tables))
(in-package #:niko)

(defparameter *app-handler* nil)

(defun start (&optional (address "localhost") (port 5000))
  (format t "Hi, I'm Niko!~%")
  (mito:connect-toplevel :porsgres
                         :database-name "inventory"
                         :host (uiop:getenv "DB_HOST")
                         :username (uiop:getenv "DB_USER")
                         :password (uiop:getenv "DB_PASS"))
  (setf *app-handler*
        (clack:clackup *app*
                       :server :woo
                       :address address
                       :port port)))

(defun stop ()
  (format t "Good night...~%")
  (clack:stop *app-handler*)
  (mito:disconnect-toplevel)
  (setf *app-handler* nil))

(defun create-tables ()
  :or-migration-command)
