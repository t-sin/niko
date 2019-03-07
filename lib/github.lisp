(defpackage #:niko/lib/github
  (:use #:cl)
  (:export #:api/notifications
           #:api/issue
           #:api/issue-comment
           #:api/user
           #:api/orgrepos
           #:all-orgrepos
           #:api/pulls
           #:all-pulls))
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
                           (quri:url-encode-params `(("all" . "true")
                                                     ,@(when since
                                                         (list (cons "since" since))))))))
          (dex:get uri
                   :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN"))))))
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

(defun api/orgrepos (orgname &optional (type "private") (page 1))
  (handler-bind
      ((error (lambda (c) (format t "~s~%" c))))
    (multiple-value-bind (res status header uri ssl)
        (let ((uri (format nil "~a/orgs/~a/repos?~a" "https://api.github.com" orgname
                           (quri:url-encode-params `(("per_page" . "100")
                                                     ("page" . ,(format nil "~a" page))
                                                     ("type" . ,type))))))
          (dex:get uri :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN"))))))
      (declare (ignore header status ssl uri))
      (mapcar (lambda (r) (getf r :|name|))
              (jojo:parse res)))))

(defun all-orgrepos (orgname type)
  (loop :named orgrepos
        :for page := 1 :then (incf page)
        :for repos := (api/orgrepos orgname type page)
        :with all-repos := nil
        :until (null repos)
        :do (setf all-repos (append all-repos repos))
        :finally (return-from orgrepos all-repos)))

(defun api/pulls (owner repo &optional (state "open") (page 1))
  (handler-bind
      ((error (lambda (c) (format t "~s~%" c))))
    (multiple-value-bind (res status header uri ssl)
        (let ((uri (format nil "~a/repos/~a/~a/pulls?~a" "https://api.github.com" owner repo
                           (quri:url-encode-params `(("state" . ,state)
                                                     ("per_page" . "100")
                                                     ("page" . ,(format nil "~a" page)))))))
          (dex:get uri :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN"))))))
      (declare (ignore header status ssl uri))
      (mapcar (lambda (r) (getf r :|number|)) (jojo:parse res)))))

(defun all-pulls (owner repo state)
  (loop
    :for page := 1 :then (incf page)
    :for pulls := (api/pulls owner repo state page)
    :with all-pulls := nil
    :until (null pulls)
    :do (setf all-pulls (append all-pulls pulls))
    :finally (return-from all-pulls all-pulls)))
