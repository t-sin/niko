(defpackage #:niko/views/main
  (:use #:cl #:lsx)
  (:import-from #:niko/app
                #:defroute
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

(defun get-view (name)
  (let ((name (format nil "~a.lsx" (string-downcase (symbol-name name)))))
    (read-lsx-file (merge-pathnames name (project-root #P"views/")))))

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
        (to-plist (add-user (assoc* "github-username" params)
                            (assoc* "slack-username" params))))))

(defroute ("/user/list" :GET)
  (let* ((lsx:*auto-escape* nil)
         (user-list (get-view :user-list))
         (rendered-body (render-object (funcall user-list :users (all-users)) nil)))
    (render-object (funcall *page-template*
                            :title "Niko - Register an user" :body rendered-body) nil)))
