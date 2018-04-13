(defpackage #:niko/tests/main
  (:use #:cl
        #:rove))
(in-package #:niko/tests/main)

(deftest test-something
  (ok (= 1 1)))
