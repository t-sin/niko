(defpackage #:niko/models
  (:use #:cl
        #:mito)
  (:export #:user
           #:user-github-id
           #:user-github-name
           #:user-slack-id
           #:user-slack-name))
(in-package #:niko/models)

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
