(defpackage #:niko/config/routes
  (:use #:cl
        #:utopian
        #:niko/config/application)
  (:import-from #:niko/api/github-webhook
                #:webhook)
  (:export #:*app*))
(in-package #:niko/config/routes)

(defvar *app* (make-instance 'application))
(clear-routing-rules *app*)

;;
;; Routing rules

(route :GET "/" "root:index")

(route :GET "/users/lists" "users:lists")
(route :GET "/users/add" "users:add")
(route :POST "/users/add" "users:add*")

(route :POST "/api/github/webhook" (lambda (env) (funcall #'webhook env)))
