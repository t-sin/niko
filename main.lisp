(defpackage #:niko
  (:use #:cl)
  (:import-from #:niko/views/main)
  (:import-from #:niko/webhook)
  (:export #:start
           #:stop
           #:generate-migrations
           #:migrate
           #:migration-status))
(in-package #:niko)

(defparameter *webhook-handler* nil)
(defparameter *admin-handler* nil)

(defun connect-db ()
  (mito:connect-toplevel :postgres
                         :database-name "inventory"
                         :host (uiop:getenv "DB_HOST")
                         :username (uiop:getenv "DB_USER")
                         :password (uiop:getenv "DB_PASS")))

(defun connect-redis ()
  (redis:connect :host (uiop:getenv "REDIS_HOST")
                 :port (parse-integer (uiop:getenv "REDIS_PORT"))))

(defun start (&optional (address "127.0.0.1") (port 5000))
  (format t "Hi, I'm Niko!~%")
  (handler-bind ((woo.ev.condition:os-error
                   (lambda (c)
                     (format *error-output* "[~a] Error occured on opening socket.~%~a~%"
                             (local-time:now) c)
                     (return-from start))))
    ;; now cannot handle other thread's condition
    ;; because of it, some conditions are not handled; e.g. port already used
    (connect-db)
    (connect-redis)
    (setf *webhook-handler*
          (clack:clackup niko/webhook:*app* :server :woo :address address :port port :debug t)
          *admin-handler*
          (clack:clackup niko/views/main:*app* :server :hunchentoot :address address :port 5001 :debug t))))

(defun stop ()
  (format t "Good night...~%")
  (clack:stop *webhook-handler*)
  (clack:stop *admin-handler*)
  (redis:disconnect)
  (mito:disconnect-toplevel)
  (setf *app-handler* nil))

(defparameter *migration-pathname* (niko/util:project-root #P"db/migrations"))

(defun generate-migrations (pathname)
  (connect-db)
  (mito:generate-migrations pathname)
  (mito:disconnect-toplevel))

(defun migrate (pathname)
  (connect-db)
  (mito:migrate pathname)
  (mito:disconnect-toplevel))

(defun migration-status (pathname)
  (connect-db)
  (format t "~a~%" (mito:migration-status pathname))
  (mito:disconnect-toplevel))
