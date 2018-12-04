(defpackage #:niko/views/main
  (:use #:cl #:lsx)
  (:import-from #:ningle
                #:not-found)
  (:import-from #:niko/app
                #:*app*
                #:defroute
                #:append-header
                #:params
                #:status-code)
  (:import-from #:niko/db/models
                #:to-plist
                #:all-users
                #:add-user)
  (:import-from #:niko/util
                #:assoc*
                #:project-root)
  (:export #:view-template))
(in-package #:niko/views/main)

(defparameter *css* (with-output-to-string (out)
                        (with-open-file (in (merge-pathnames "style.css" (project-root #P"views/")))
                          (loop
                            :for line := (read-line in nil :eof)
                            :until (eq line :eof)
                            :do (format out "~a~%" line)))))

(defroute ("/style.css" :GET) *css*)

(defun get-view (name)
  (let ((name (format nil "~a.lsx" (string-downcase (symbol-name name)))))
    (read-lsx-file (merge-pathnames name (project-root #P"views/")))))

(defparameter *page-not-found* (get-view :not-found))

(defmethod not-found ((app (eql *app*)))
  (let ((lsx:*auto-escape* nil)
        (path (lack.request:request-path-info ningle:*request*)))
    (render-object (funcall *page-template*
                            :title "Niko - Register an user"
                            :body (funcall *page-not-found* :path path)) nil)))

(defparameter *page-template* (get-view :template))

(defroute ("/" :GET)
  (let ((lsx:*auto-escape* nil))
    (render-object (funcall *page-template*
                            :title "Niko - Not-a-cat Slack bot"
                            :body (get-view :root)) nil)))

(defroute ("/user/add" :GET)
  (let ((lsx:*auto-escape* nil))
    (render-object (funcall *page-template*
                            :title "Niko - Register an user"
                            :body (get-view :user-add)) nil)))

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
                    (assoc* "slack-username" params))
          (append-header :location "/user/list")
          (setq status-code 303)))))

(defroute ("/user/list" :GET)
  (let* ((lsx:*auto-escape* nil)
         (user-list (get-view :user-list))
         (rendered-body (render-object (funcall user-list :users (all-users)) nil)))
    (render-object (funcall *page-template*
                            :title "Niko - User List" :body rendered-body) nil)))
