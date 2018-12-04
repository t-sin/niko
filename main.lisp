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
  (handler-bind ((woo.ev.condition:os-error
                   (lambda (c)
                     (format *error-output* "[~a] Error occured on opening socket.~%~a~%"
                             (local-time:now) c)
                     (return-from start))))
    ;; now cannot handle other thread's condition
    ;; because of it, some conditions are not handled; e.g. port already used
    (mito:connect-toplevel :postgres
                           :database-name "inventory"
                           :host (uiop:getenv "DB_HOST")
                           :username (uiop:getenv "DB_USER")
                           :password (uiop:getenv "DB_PASS"))
    (setf *app-handler*
          (clack:clackup *app*
                         :server :woo
                         :address address
                         :port port
                         :debug t))))

(defun stop ()
  (format t "Good night...~%")
  (clack:stop *app-handler*)
  (mito:disconnect-toplevel)
  (setf *app-handler* nil))

(defun create-tables ()
  :or-migration-command)
