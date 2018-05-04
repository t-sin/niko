(defpackage #:niko/controllers/users
  (:use #:cl
        #:utopian)
  (:import-from #:niko/models
                #:users
                #:users-github-id
                #:users-github-name
                #:users-slack-id
                #:users-slack-name)
  (:import-from #:niko/lib/github
                #:api/user)
  (:export #:lists))
(in-package #:niko/controllers/users)

(defun lists (params)
  (declare (ignore params))
  (render nil :template "users/lists.html.dj"))

(defun add (params)
  (declare (ignore params))
  (render nil :template "users/add.html.dj"))

(defun add- (params)
  "aaaaaaaaaaaaaaaa")
