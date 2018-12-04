(defpackage #:niko/util
  (:use #:cl)
  (:export #:assoc*
           #:*project-name*
           #:project-root))
(in-package #:niko/util)

(defun assoc* (str alist)
  (cdr (assoc str alist :test #'string=)))

(defparameter *project-name* "niko")

(defun project-root (pathname)
  (merge-pathnames pathname
                   (asdf:component-pathname (asdf:find-system *project-name*))))
