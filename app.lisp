(push (uiop:pathname-directory-pathname *load-pathname*)
      asdf:*central-registry*)
(ql:quickload :niko :silent t)

(defpackage #:niko/app
  (:use #:cl
        #:niko
        #:utopian)
  (:import-from #:lack
                #:builder)
  (:import-from #:mito))
(in-package #:niko/app)

(apply #'mito:connect-toplevel (connection-settings :maindb))

(builder
 (:static
  :path "/public/"
  :root (project-path #P"public/"))
 :accesslog
 (unless (productionp)
   :clack-errors)
 (when (config :error-log)
   `(:backtrace :output ,(config :error-log)))
 :session
 :csrf
 *app*)
