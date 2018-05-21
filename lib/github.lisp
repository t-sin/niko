(defpackage #:niko/lib/github
  (:use #:cl)
  (:export #:api/notifications
           #:api/issue
           #:api/issue-comment
           #:api/user))
(in-package #:niko/lib/github)

;;;; APIs
(defun api/notifications (&optional since)
  (handler-bind
      ((error
        (lambda (c)
          (format t "~s~%" c)
)))
    (multiple-value-bind (res status header uri ssl)
        (let ((uri (format nil "https://api.github.com/notifications?~a"
                           (quri:url-encode-params '(("all" . "true"))))))
          (dex:get uri
                   :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN")))
                              ,(when since
                                 (cons "If-Modifed-Since"  since)))))
      (declare (ignore status ssl uri))
      (values
       (jojo:parse res)
       (gethash "last-modified" header)
       (gethash "x-poll-interval" header)))))

(defun api/issue (owner repo id)
  (handler-bind
      ((error
         (lambda (c)
           (format t "~s~%" c))))
    (multiple-value-bind (res status header uri ssl)
        (let ((uri (format nil "~a/repos/~a/~a/issues/~a"
                           "https://api.github.com"
                           owner repo id)))
          (dex:get uri :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN"))))))
      (declare (ignore header status ssl uri))
      (jojo:parse res))))

(defun api/issue-comment (owner repo id)
  (handler-bind
      ((error
         (lambda (c)
           (format t "~s~%" c))))
    (multiple-value-bind (res status header uri ssl)
        (let ((uri (format nil "~a/repos/~a/~a/issues/comments/~a"
                           "https://api.github.com"
                           owner repo id)))
          (dex:get uri :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN"))))))
      (declare (ignore header status ssl uri))
      (jojo:parse res))))

(defun api/user (username)
  (handler-bind
      ((error
         (lambda (c)
           (format t "~s~%" c))))
    (multiple-value-bind (res status header uri ssl)
        (let ((uri (format nil "~a/users/~a" "https://api.github.com" username)))
          (dex:get uri :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN"))))))
      (declare (ignore header status ssl uri))
      (let ((parsed (jojo:parse res)))
        (values (getf parsed :|id|)
                (getf parsed :|login|))))))
