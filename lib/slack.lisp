(defpackage #:niko/lib/slack
  (:use #:cl)
  (:export #:api/channel-id
           #:api/user-ids
           #:api/post-message))
(in-package #:niko/lib/slack)

;;;; APIs
(defun api/channel-id (channel-name)
  (let ((ret (dex:get "https://slack.com/api/channels.list"
                      :headers `(("Authorization" . ,(format nil "Bearer ~a" (uiop:getenv "SLACK_TOKEN")))))))
    (getf (find channel-name (getf (jojo:parse ret) :|channels|)
                :key (lambda (ch) (getf ch :|name|))
                :test #'string=)
          :|id|)))

(defun api/user-ids (usernames)
  (let ((ret (dex:get "https://slack.com/api/users.list"
                      :headers `(("Authorization" . ,(format nil "Bearer ~a" (uiop:getenv "SLACK_TOKEN")))))))
    (mapcar (lambda (user) (getf user :|id|))
            (remove-if-not (lambda (user) (member (getf user :|name|) usernames :test #'string=))
                           (getf (jojo:parse ret) :|members|)))))

(defun api/post-message (channel text &optional (mention-to nil))
  (flet ((mention-str (text) (format nil "~{<@~a> ~}~a" mention-to text)))
    (let ((ret (dex:post "https://slack.com/api/chat.postMessage"
                         :content (jojo:to-json (list :|channel| channel :|text| (mention-str text)))
                         :headers `(("Content-Type" . "application/json")
                                    ("Authorization" . ,(format nil "Bearer ~a" (uiop:getenv "SLACK_TOKEN")))))))
      (jojo:parse ret))))
