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
