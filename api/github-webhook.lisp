(defpackage #:niko/api/github-webhook
  (:use #:cl)
  (:import-from #:niko/lib/slack
                #:api/channel-id
                #:api/user-ids
                #:api/post-message)
  (:import-from #:niko/models/users
                #:users
                #:users-slack-id)
  (:import-from #:jonathan
                #:parse)
  (:export #:webhook))
(in-package #:niko/api/github-webhook)

(defun all-mentions-from (text)
  (mapcar (lambda (s) (subseq s 1))
          (ppcre:all-matches-as-strings "@[^ ]+" text)))

(defun to-slack-user-id (github-usernames)
  (let ((users (mito:select-dao 'users
                 (sxql:where (:in :github-name github-usernames)))))
    (mapcar #'users-slack-id
            users)))

(defun generate-message (verb type title url body)
  (format nil "~% You are ~a on the ~a `~a`~%~a~%~{~^> ~a~}"
          verb type title url (split-sequence:split-sequence #\newline body)))

(defun handle-issues (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (issue (getf payload :|issue|))
         (assignee (getf payload :|assignee|))
         (mentioned (remove-duplicates (all-mentions-from (getf issue :|body|))
                                       :test #'string=))
         (mentioned-slack-ids (to-slack-user-id mentioned)))
    (if (and (string= (getf payload :|action|) "assigned") assignee)
        (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                          (generate-message "assigned" "issue" (getf issue :|title|)
                                            (getf issue :|html_url|)
                                            (getf issue :|body|))
                          (to-slack-user-id (list (getf assignee :|login|))))
        (when mentioned-slack-ids
          (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                            (generate-message "commented" "issue" (getf issue :|title|)
                                              (getf issue :|html_url|)
                                              (getf issue :|body|))
                            mentioned-slack-ids)))
    "handled issue"))

(defun handle-issue-comment (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (issue (getf payload :|issue|))
         (comment (getf payload :|comment|))
         (mentioned (remove-duplicates (all-mentions-from (getf comment :|body|))
                                       :test #'string=))
         (mentioned-slack-ids (to-slack-user-id mentioned)))
    (when mentioned-slack-ids
      (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                        (generate-message "commented" "issue comment" (getf issue :|title|)
                                          (getf comment :|html_url|)
                                          (getf comment :|body|))
                        mentioned-slack-ids))
    "handled issue-comment"))

(defun handle-pull-request (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (pr (getf payload :|pull_request|))
         (assignee (getf payload :|assignee|))
         (mentioned (remove-duplicates (all-mentions-from (getf pr :|body|))
                                       :test #'string=))
         (mentioned-slack-ids (to-slack-user-id mentioned)))
    (if (and (string= (getf payload :|action|) "assigned") assignee)
        (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                          (generate-message "assigned" "Pull-Request"
                                            (getf pr :|title|) (getf pr :|html_url|) (getf pr :|body|))
                          (to-slack-user-id (list (getf assignee :|login|))))
        (when mentioned-slack-ids
          (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                            (generate-message "commented" "Pull-Request"
                                              (getf pr :|title|) (getf pr :|html_url|) (getf pr :|body|))
                            mentioned-slack-ids)))
    "handled pull-request"))

(defun handle-pull-request-review (env)
  (let ((payload (parse (cdr (assoc "payload" env :test #'string=)))))
    (when (string= (getf payload :|action|) "submitted")
      (let* ((pr (getf payload :|pull_request|))
             (mentioned (remove-duplicates (all-mentions-from (getf payload :|body|))
                                           :test #'string=))
             (mentioned-slack-ids (to-slack-user-id mentioned)))
        (when mentioned-slack-ids
          (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                            (generate-message "mentioned" "PR Review"
                                              (getf pr :|title|) (getf payload :|html_url|) "")
                            mentioned-slack-ids))
        "handled pull-request review"))))

(defun handle-pull-request-review-comment (env)
  (let ((payload (parse (cdr (assoc "payload" env :test #'string=)))))
    (when (string= (getf payload :|action|) "created")
      (let* ((pr (getf payload :|pull_request|))
             (comment (getf payload :|comment|))
             (mentioned (remove-duplicates (all-mentions-from (getf comment :|body|))
                                           :test #'string=))
             (mentioned-slack-ids (to-slack-user-id mentioned)))
        (when mentioned-slack-ids
          (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                            (generate-message "mentioned" "PR Review"
                                              (getf pr :|title|) (getf comment :|html|) "")
                            mentioned-slack-ids))
        "handled pull-request review comment"))))

(defun webhook (env)
  (let ((event-type (gethash "x-github-event"
                             (getf (lack.request:request-env utopian:*request*) :headers))))
    (format t "event-type: ~a~%" event-type)
    (when (stringp event-type)
      (cond ((string= event-type "ping") "hello GitHub!")
        ((string= event-type "issues") (handle-issues env))
        ((string= event-type "pull_request") (handle-pull-request env))
        ((string= event-type "pull_request_review") (handle-pull-request-review env))
        ((string= event-type "pull_request_review_comment") (handle-pull-request-review-comment env))
        ((string= event-type "issue_comment") (handle-issue-comment env))
        (t (format t "do nothing anymore~%"))))))
