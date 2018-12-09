(defpackage #:niko/db/models
  (:use #:cl
        #:sxql
        #:mito)
  (:import-from #:niko/lib/github
                #:api/user)
  (:import-from #:niko/lib/slack
                #:api/user-ids)
  (:export #:to-plist

           #:user
           #:user-github-id
           #:user-github-name
           #:user-slack-id
           #:user-slack-name

           #:all-users
           #:add-user))
(in-package #:niko/db/models)

(defgeneric to-plist (dao))

(defclass user ()
  ((github-id :col-type (:varchar 256)
              :initarg :github-id
              :accessor user-github-id)
   (github-name :col-type (:varchar 256)
                :initarg :github-name
                :accessor user-github-name)
   (slack-id :col-type (:varchar 256)
             :initarg :slack-id
             :accessor user-slack-id)
   (slack-name :col-type (:varchar 256)
               :initarg :slack-name
               :accessor user-slack-name)
   (channel :col-type (:varchar 128)
            :initarg :channel
            :accessor user-channel))
  (:metaclass dao-table-class))

(defmethod to-plist ((dao user))
  (list :id (object-id dao)
        :github-id (user-github-id dao)
        :github-name (user-github-name dao)
        :slack-id (user-slack-id dao)
        :slack-name (user-slack-name dao)
        :channel (user-channel dao)))

(defun all-users ()
  (mapcar #'to-plist (select-dao 'user)))

(defun add-user (github-name slack-name &optional channel)
  (let ((user (make-instance 'user :github-name github-name :slack-name slack-name)))
    (multiple-value-bind (id name)
        (api/user github-name)
      (setf (user-github-id user) id)
      (setf (user-github-name user) name))
    (let ((id (first (api/user-ids (list slack-name)))))
      (setf (user-slack-id user) id)
      (setf (user-slack-name user) slack-name))
    (when channel
      (setf (user-channel user) channel))
    (save-dao user)))

;; TODO: update-user

;; TODO: delete-user
