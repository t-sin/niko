(defpackage #:niko/util
  (:use #:cl)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:ningle
                #:<app>)
  (:export #:*project-name*
           #:project-root
           #:version
           #:assoc*
           #:append-header
           #:defroute
           #:defapi
           #:status-code
           #:params))
(in-package #:niko/util)

(defun assoc* (str alist)
  (cdr (assoc str alist :test #'string=)))

(defparameter *project-name* "niko")

(defun project-root (pathname)
  (merge-pathnames pathname
                   (asdf:component-pathname (asdf:find-system *project-name*))))
(defun version ()
  (asdf:component-version (asdf:find-system *project-name*)))

(defun to-json* (obj)
  (let ((jojo:*null-value* :null)
        (jojo:*false-value* :false)
        (jojo:*empty-array-value* :[])
        (jojo:*empty-object-value* :{}))
    (jojo:to-json obj)))

(defmacro append-header (name value)
  `(setf (lack.response:response-headers ningle:*response*)
        (append (lack.response:response-headers ningle:*response*)
                (list ,name ,value))))

(defmacro defroute ((path method) &body body)
  (let ((*app* (intern "*APP*" *package*))
        (params (intern "PARAMS" *package*)))
    `(setf (ningle:route ,*app* ,path :method ,method)
           (lambda (,params)
             (declare (ignorable ,params))
             (symbol-macrolet ((status-code (lack.response:response-status ningle:*response*)))
               ,@body)))))

(defmacro with-json (&body body)
  `(let ((result (progn ,@body)))
     (append-header :content-type "application/json")
     (append-header :access-control-allow-origin "*")
     (append-header :access-control-allow-headres "Content-type, Authorization")
     (to-json* result)))

(defmacro defapi ((path method) &body body)
  `(progn
     (defroute (,path :OPTIONS) (with-json "OK"))
     (defroute (,path ,method) (with-json ,@body))))
