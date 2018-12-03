(defpackage #:niko/util
  (:use #:cl)
  (:export #:*project-name*
           #:project-root))
(in-package #:niko/util)

(defparameter *project-name* "niko")

(defun project-root (pathname)
  (merge-pathnames pathname
                   (asdf:component-pathname (asdf:find-system *project-name*))))
