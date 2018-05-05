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
  (:import-from #:niko/lib/slack
                #:api/user-ids)
  (:export #:lists))
(in-package #:niko/controllers/users)

(defun to-plist (users)
  (loop
    :for u :in users
    :collect (list :id (mito:object-id u)
                   :github-id (users-github-id u)
                   :github-name (users-github-name u)
                   :slack-id (users-slack-id u)
                   :slack-name (users-slack-name u))))

(defun lists (params)
  (declare (ignore params))
  (render (list :users (to-plist (mito:select-dao 'users)))
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
    (let ((user-ids (api/user-ids (list slack-name))))
      (setf (users-slack-id user) (first user-ids))
      (setf (users-slack-name user) slack-name))

    (mito:save-dao user)
    (render (list :users (mito:select-dao 'users))
            :template "users/lists.html.dj")))
