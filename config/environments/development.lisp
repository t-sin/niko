(defpackage #:niko/config/environments/development
  (:use #:cl
        #:utopian))
(in-package #:niko/config/environments/development)

`(:databases
  ((:maindb . (:sqlite3
               :database-name ,(project-path #P"db/development.db"))))
  :error-log ,(project-path #P"log/error.log"))
