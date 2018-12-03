(defpackage #:niko/app
  (:use #:cl)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:ningle
                #:<app>)
  (:import-from #:niko/util
                #:project-root)
  (:export #:*napp*
           #:*app*))
(in-package #:niko/app)

(defparameter *napp* (make-instance '<app>))

(defparameter *app*
  (lack:builder
   (:static :path "/public/" :root (project-root #P"public/"))
   *napp*))
