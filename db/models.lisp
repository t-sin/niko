(defpackage #:niko/models
  (:use #:cl
        #:sxql
        #:mito)
  (:import-from #:niko/lib/github
                #:api/user)
  (:import-from #:niko/lib/slack
                #:api/user-ids)
  (:export #:user
           #:user-github-id
           #:user-github-name
           #:user-slack-id
           #:user-slack-name

           #:all-users
           #:add-user))
(in-package #:niko/models)

(defgeneric to-plist (dao))

(defclass user ()
  ((github-id :col-type :string
              :initarg :github-id
              :accessor user-github-id)
   (github-name :col-type :string
                :initarg :github-name
                :accessor user-github-name)
   (slack-id :col-type :string
             :initarg :slack-id
             :accessor user-slack-id)
   (slack-name :col-type :string
               :initarg :slack-name
               :accessor user-slack-name))
  (:metaclass dao-table-class))

(defmethod to-plist ((dao user))
  (list :id (object-id dao)
        :github-id (user-github-id dao)
        :github-name (user-github-name dao)
        :slack-id (user-slack-id dao)
        :slack-name (user-slack-name dao)))

(defun all-users ()
  (mapcar #'to-plist (select-dao 'user)))

(defun add-user (github-name slack-name)
  (let ((user (make-instance 'user :github-name github-name :slack-name slack-name)))
    (multiple-value-bind (id name)
        (api/user github-name)
      (setf (user-github-id user) id)
      (setf (user-github-name user) name))
    (let ((id (first (api/user-ids (list slack-name)))))
      (setf (user-slack-id user) id)
      (setf (user-slack-name user) slack-name))
    (save-dao user)))

;; TODO: update-user

;; TODO: delete-user
