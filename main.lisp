(defpackage #:niko
  (:use #:cl)
  (:import-from #:niko/db/models
                #:user
                #:user-slack-id)
  (:import-from #:niko/lib/github
                #:all-orgrepos
                #:all-pulls
                #:api/review-requests)
  (:import-from #:niko/lib/slack
                #:api/post-message)
  (:import-from #:niko/app
                #:*app*)
  (:import-from #:niko/views/main)
  (:export #:start
           #:stop
           #:generate-migrations
           #:migrate
           #:migration-status
           #:review-request))
(in-package #:niko)

(defparameter *app-handler* nil)

(defun connect-db ()
  (mito:connect-toplevel :postgres
                         :database-name "inventory"
                         :host (uiop:getenv "DB_HOST")
                         :username (uiop:getenv "DB_USER")
                         :password (uiop:getenv "DB_PASS")))

(defun start (&optional (address "localhost") (port 5000))
  (format t "Hi, I'm Niko!~%")
  (handler-bind ((woo.ev.condition:os-error
                   (lambda (c)
                     (format *error-output* "[~a] Error occured on opening socket.~%~a~%"
                             (local-time:now) c)
                     (return-from start))))
    ;; now cannot handle other thread's condition
    ;; because of it, some conditions are not handled; e.g. port already used
    (connect-db)
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

(defun review-request (org)
  (let* ((repos (all-orgrepos org ""))
         (pulls (loop
                  :for repo :in repos
                  :for pulls := (all-pulls org repo "open")
                  :nconc pulls))
         (channel (uiop:getenv "SLACK_CHANNEL"))
         (message-template "~%Hey! You are requested review in PR `~a`~%~a~%Check it out🐱"))
    (flet ((notify (pull requested-users)
             (api/post-message channel
                               (format nil message-template (getf pull :|title|) (getf pull :|html_url|))
                               requested-users)))
      (loop
        :for pull :in pulls
        :for requested := (api/review-requests org
                                               (getf (getf (getf pull :|head|) :|repo|) :|name|)
                                               (getf pull :|number|))
        :for slack-users := (mapcar #'user-slack-id
                                    (mito:select-dao 'user
                                      (sxql:where (:in :github-name requested-users))))
        :do (unless (null slack-users)
              (notify pull slack-users))))))
