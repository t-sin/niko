(defpackage #:niko/views/main
  (:use #:cl #:lsx)
  (:import-from #:ningle
                #:not-found)
  (:import-from #:niko/db/models
                #:to-plist
                #:all-users
                #:add-user)
  (:import-from #:niko/util
                #:assoc*
                #:project-root
                #:version
                #:defroute
                #:defapi
                #:append-header
                #:params
                #:status-code)
  (:export #:*app*
           #:view-template))
(in-package #:niko/views/main)

(defparameter *app* (make-instance 'ningle:<app>))

(defparameter *css* (with-output-to-string (out)
                        (with-open-file (in (merge-pathnames "style.css" (project-root #P"views/"))
                                            :external-format :utf-8)
                          (loop
                            :for line := (read-line in nil :eof)
                            :until (eq line :eof)
                            :do (format out "~a~%" line)))))

(defroute ("/style.css" :GET)
  (append-header :content-type "text/css")
  *css*)

(defun get-view (name)
  (let ((name (format nil "~a.lsx" (string-downcase (symbol-name name)))))
    (read-lsx-file (merge-pathnames name (project-root #P"views/")))))

(defparameter *page-map*
  (list :template (get-view :template)
        :not-found (get-view :not-found)
        :root (get-view :root)
        :user-add (get-view :user-add)
        :user-list (get-view :user-list)))

(defmethod not-found ((app (eql *app*)))
  (let ((lsx:*auto-escape* nil)
        (path (lack.request:request-path-info ningle:*request*)))
    (render-object (funcall (getf *page-map* :template)
                            :title "Niko - Register an user" :version (version)
                            :body (funcall (getf *page-map* :not-found) :path path)) nil)))

(defroute ("/" :GET)
  (let ((lsx:*auto-escape* nil))
    (render-object (funcall (getf *page-map* :template)
                            :title "Niko - Not-a-cat Slack bot" :version (version)
                            :body (getf *page-map* :root)) nil)))

(defroute ("/user/add" :GET)
  (let ((lsx:*auto-escape* nil))
    (render-object (funcall (getf *page-map* :template)
                            :title "Niko - Register an user" :version (version)
                            :body (getf *page-map* :user-add)) nil)))

(defun validate-user-add (params)
  t)

(defroute ("/user/add" :POST)
  (let ((result (validate-user-add params)))
    (if (typep result 'string)
        (progn
          (setq status-code 400)
          result)
        (progn
          (add-user (assoc* "github-username" params)
                    (assoc* "slack-username" params)
                    (assoc* "slack-channel" params))
          (append-header :location "/user/list")
          (setq status-code 303)
          "/user/list"))))

(defroute ("/user/list" :GET)
  (let* ((lsx:*auto-escape* nil)
         (rendered-body (render-object
                         (funcall (getf *page-map* :user-list) :users (all-users)) nil)))
    (render-object (funcall (getf *page-map* :template)
                            :title "Niko - User List"
                            :body rendered-body
                            :version (version)) nil)))
