(defpackage #:niko/views/main
  (:use #:cl #:lsx)
  (:import-from #:niko/app
                #:defroute)
  (:import-from #:niko/util
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

(defparameter *users*
  '((:id 1 :github-id 1234 :github-name "t-sin" :slack-id 12 :slack-name "t-sin")
    (:id 2 :github-id 111 :github-name "test" :slack-id 345 :slack-name "hoge")))
(defroute ("/user/list" :GET)
  (let* ((lsx:*auto-escape* nil)
         (user-list (get-view :user-list))
         (rendered-body (render-object (funcall user-list :users *users*) nil)))
    (render-object (funcall *page-template*
                            :title "Niko - Register an user" :body rendered-body) nil)))
