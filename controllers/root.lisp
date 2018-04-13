(defpackage #:niko/controllers/root
  (:use #:cl
        #:utopian)
  (:export #:index))
(in-package #:niko/controllers/root)

(defun index (params)
  (declare (ignore params))
  (render nil :template :index))
