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
   (slack-id :col-type :string
             :initarg :slack-id
             :accessor users-slack-id))
  (:metaclass dao-table-class))
