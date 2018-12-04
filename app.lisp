(defpackage #:niko/app
  (:use #:cl)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:ningle
                #:<app>)
  (:import-from #:niko/util
                #:project-root)
  (:export #:*napp*
           #:*app*
           #:assoc*
           #:append-header
           #:defroute
           #:defapi
           #:status-code
           #:params))
(in-package #:niko/app)

(defparameter *napp* (make-instance '<app>))

(defparameter *app*
  (lack:builder
   (:static :path "/public/" :root (project-root #P"public/"))
   *napp*))

(defun to-json* (obj)
  (let ((jojo:*null-value* :null)
        (jojo:*false-value* :false)
        (jojo:*empty-array-value* :[])
        (jojo:*empty-object-value* :{}))
    (to-json obj)))

(defmacro append-header (name value)
  `(setf (lack.response:response-headers ningle:*response*)
        (append (lack.response:response-headers ningle:*response*)
                (list ,name ,value))))

(defmacro defroute ((path method) &body body)
  `(setf (ningle:route niko/app:*napp* ,path :method ,method)
         (lambda (params)
           (declare (ignorable params))
           (symbol-macrolet ((status-code (lack.response:response-status ningle:*response*)))
             ,@body))))

(defmacro with-json (&body body)
  `(let ((result (progn ,@body)))
     (append-header :content-type "application/json")
     (append-header :access-control-allow-origin "*")
     (append-header :access-control-allow-headres "Content-type, Authorization")
     (let ((result (progn ,@body)))
       (to-json* result))))

(defmacro defapi ((path method) &body body)
  `(progn
     (defroute (,path :OPTIONS) (with-json "OK"))
     (defroute (,path ,method) (with-json ,@body))))
