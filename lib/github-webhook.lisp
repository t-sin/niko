(defpackage #:niko/lib/github-webhook
  (:use #:cl)
  (:import-from #:niko/lib/slack
                #:api/channel-id
                #:api/user-ids
                #:api/post-message)
  (:import-from #:niko/db/models
                #:user
                #:user-slack-id
                #:user-channel)
  (:import-from #:niko/util
                #:assoc*)
  (:import-from #:redis
                #:red-get
                #:red-set
                #:red-expire)
  (:import-from #:jonathan
                #:parse)
  (:export #:webhook))
(in-package #:niko/lib/github-webhook)

(defun all-mentions-from (text)
  (mapcar (lambda (s) (subseq s 1))
          (ppcre:all-matches-as-strings "@[^ ]+" text)))

(defun to-users (github-usernames)
  (mito:select-dao 'user
    (sxql:where (:in :github-name github-usernames))))

(defun generate-message (verb type title url body)
  (format nil "~% You are ~a on the ~a `~a`~%~a~%~{~^> ~a~}"
          verb type title url
          (split-sequence:split-sequence #\newline (if (> (length body) 300) (subseq body 0 300) body))))

(defun make-mapping (users)
  (let ((default-channel (api/channel-id (uiop:getenv "SLACK_CHANNEL")))
        (mapping))
    (flet ((to-keyword (str)
             (intern (or str "nil") :keyword)))
      (loop
        :for u :in users
        :do (if (null (user-channel u))
                (push u (getf mapping (to-keyword default-channel)))
                (push u (getf mapping (to-keyword (api/channel-id (user-channel u)))))))
    mapping)))

(defun post-messages (text mapping)
  (loop
    :for (channel users) :on mapping :by #'cddr
    :do (api/post-message channel text (mapcar #'user-slack-id users))))

(defmacro with-payload ((var keys) &body body)
  `(let* ((,var (parse (assoc* "payload" params)))
          ,@(mapcar (lambda (k) (list k (list 'getf var (intern (string-downcase (symbol-name k)) :keyword))))
                      keys))
     ,@body))

(defmacro unless-key-exists (key &body body)
  (let (($key (gensym)))
    `(let ((,$key ,key))
       (when (null (red-get ,$key))
         (red-set ,$key "nya~ (processed)")
         (red-expire ,$key 30)
         ,@body))))

(defun handle-issues (params)
  (with-payload (payload (action issue assignee))
    (let ((mentioned (remove-duplicates (all-mentions-from (getf issue :|body|))
                                        :test #'string=)))
      (if (and (string= action "assigned") assignee)
          (when assignee
            (unless-key-exists (format nil "issues:~a" (getf issue :|id|))
              (post-messages (generate-message "assigned" "issue" (getf issue :|title|)
                                               (getf issue :|html_url|) (getf issue :|body|))
                             (make-mapping (to-users (list (getf assignee :|login|)))))))
          (when (and (string= action "opened") mentioned)
            (post-messages (generate-message "commented" "issue" (getf issue :|title|)
                                             (getf issue :|html_url|) (getf issue :|body|))
                           (make-mapping (to-users mentioned)))))
      (format t "handled issue~%"))))

(defun handle-issue-comment (params)
  (with-payload (payload (action issue comment))
    (let ((mentioned (remove-duplicates (all-mentions-from (getf comment :|body|))
                                        :test #'string=)))
      (when (and (string= action "created") mentioned)
        (post-messages (generate-message "commented" "issue comment" (getf issue :|title|)
                                         (getf comment :|html_url|) (getf comment :|body|))
                       (make-mapping (to-users mentioned))))
      (format t "handled issue-comment~%"))))

(defun handle-pull-request (params)
  (with-payload (payload (action pull_request assignee))
    (let ((mentioned (remove-duplicates (all-mentions-from (getf pull_request :|body|))
                                        :test #'string=)))
      (cond ((and (string= action "assigned") assignee)
             (when assignee
               (unless-key-exists (format nil "pulls:~a" (getf pull_request :|id|))
                 (post-messages (generate-message "assigned" "Pull-Request" (getf pull_request :|title|)
                                                  (getf pull_request :|html_url|) (getf pull_request :|body|))
                                (make-mapping (to-users (list (getf assignee :|login|))))))))
            ((and (string= action "review_requested") (getf pull_request :|requested_reviewers|))
             (let ((reviewers (mapcar (lambda (rev) (getf rev :|login|))
                                      (getf pull_request :|requested_reviewers|))))
               (unless-key-exists (format nil "pulls:reivew~a" (getf pull_request :|id|)))
                 (post-messages (generate-message "assigned" "Pull-Request" (getf pull_request :|title|)
                                                  (getf pull_request :|html_url|) (getf pull_request :|body|))
                                (make-mapping (to-users reviewers)))))
            ((and (string= action "opened") mentioned)
             (post-messages (generate-message "commented" "Pull-Request" (getf pull_request :|title|)
                                              (getf pull_request :|html_url|) (getf pull_request :|body|))
                            (make-mapping (to-users mentioned)))))
      (format t "handled pull-request~%"))))

(defun handle-pull-request-review (params)
  (with-payload (payload (action pull_request review))
    (when (string= action "submitted")
      (let ((mentioned (remove-duplicates (all-mentions-from (getf review :|body|))
                                          :test #'string=)))
        (when mentioned
          (post-messages (generate-message "mentioned" "PR Review" (getf pull_request :|title|)
                                           (getf review :|html_url|) (getf review :|body|))
                         (make-mapping (to-users mentioned))))
        (format t "handled pull-request review~%")))))

(defun handle-pull-request-review-comment (params)
  (with-payload (payload (action pull_request comment))
    (when (string= action "created")
      (let ((mentioned (remove-duplicates (all-mentions-from (getf comment :|body|))
                                          :test #'string=)))
        (when mentioned
          (post-messages (generate-message "mentioned" "PR Review comment" (getf pull_request :|title|)
                                           (getf comment :|html_url|) (getf comment :|body|))
                         (make-mapping (to-users mentioned))))
        (format t "handled pull-request review comment~%")))))

(defun webhook (params)
  (let ((event-type (gethash "x-github-event"
                             (getf (lack.request:request-env ningle:*request*) :headers))))
    (format t "event-type: ~a~%" event-type)
    (when (stringp event-type)
      (cond ((string= event-type "ping") "hello GitHub!")
            ((string= event-type "issues") (handle-issues params))
            ((string= event-type "pull_request") (handle-pull-request params))
            ((string= event-type "pull_request_review") (handle-pull-request-review params))
            ((string= event-type "pull_request_review_comment") (handle-pull-request-review-comment params))
            ((string= event-type "issue_comment") (handle-issue-comment params))
            (t (format t "do nothing anymore~%"))))))
