(defpackage #:niko/config/routes
  (:use #:cl
        #:utopian
        #:niko/config/application)
  (:export #:*app*))
(in-package #:niko/config/routes)

(defvar *app* (make-instance 'application))
(clear-routing-rules *app*)

;;
;; Routing rules

(route :GET "/" "root:index")
