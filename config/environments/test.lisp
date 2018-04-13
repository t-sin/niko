(defpackage #:niko/config/environments/test
  (:use #:cl
        #:utopian))
(in-package #:niko/config/environments/test)

`(:databases
  ((:maindb . (:sqlite3
               :database-name ,(project-path #P"db/test.db")))))
