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
  (render (list :users (mito:select-dao 'users))
          :template "users/lists.html.dj"))

(defun add (params)
  (declare (ignore params))
  (render nil :template "users/add.html.dj"))

(defun add* (params)
  (let ((github-name (cdr (assoc "github-username" params :test #'string=)))
        (slack-name (cdr (assoc "slack-username" params :test #'string=)))
        (user (make-instance 'users)))

    (multiple-value-bind (id name)
        (api/user github-name)
      (setf (users-github-id user) id)
      (setf (users-github-name user) name))

    ;;; TODO: slack API
    (setf (users-slack-id user) "hoge-id")
    (setf (users-slack-name user) "hoge-user")

    (mito:save-dao user)
    (render (list :users (mito:select-dao 'users))
            :template "users/lists.html.dj")))
