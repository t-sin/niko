(defpackage #:niko/models/users
  (:use #:cl
        #:mito)
  (:export #:users
           #:users-github-id
           #:users-slack-id))
(in-package #:niko/models/users)

(defclass users ()
  ((github-id :col-type :string
              :initarg :github-id
              :accessor users-github-id)
   (github-name :col-type :string
                :initarg :github-name
                :accessor users-github-name)
   (slack-id :col-type :string
             :initarg :slack-id
             :accessor users-slack-id)
   (slack-name :col-type :string
               :initarg :slack-name
               :accessor users-slack-name))
  (:metaclass dao-table-class))
