(defpackage #:niko/lib/github-webhook
  (:use #:cl)
  (:import-from #:niko/lib/slack
                #:api/channel-id
                #:api/user-ids
                #:api/post-message)
  (:import-from #:niko/db/models
                #:user
                #:user-slack-id)
  (:import-from #:jonathan
                #:parse)
  (:export #:webhook))
(in-package #:niko/lib/github-webhook)

(defun all-mentions-from (text)
  (mapcar (lambda (s) (subseq s 1))
          (ppcre:all-matches-as-strings "@[^ ]+" text)))

(defun to-slack-user-id (github-usernames)
  (let ((users (mito:select-dao 'user
                 (sxql:where (:in :github-name github-usernames)))))
    (mapcar #'users-slack-id
            users)))

(defun generate-message (verb type title url body)
  (format nil "~% You are ~a on the ~a `~a`~%~a~%~{~^> ~a~}"
          verb type title url
          (split-sequence:split-sequence #\newline (if (> (length body) 300) (subseq body 0 300) body))))

(defun handle-issues (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (action (getf payload :|action|))
         (issue (getf payload :|issue|))
         (assignee (getf payload :|assignee|))
         (mentioned (remove-duplicates (all-mentions-from (getf issue :|body|))
                                       :test #'string=))
         (mentioned-slack-ids (to-slack-user-id mentioned)))
    (if (and (string= action "assigned") assignee)
        (let ((assignee (to-slack-user-id (list (getf assignee :|login|)))))
          (when assignee
            (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                              (generate-message "assigned" "issue" (getf issue :|title|)
                                                (getf issue :|html_url|)
                                                (getf issue :|body|))
                              assignee)))
        (when (and (string= action "opened") mentioned-slack-ids)
          (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                            (generate-message "commented" "issue" (getf issue :|title|)
                                              (getf issue :|html_url|)
                                              (getf issue :|body|))
                            mentioned-slack-ids)))
    "handled issue"))

(defun handle-issue-comment (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (action (getf payload :|action|))
         (issue (getf payload :|issue|))
         (comment (getf payload :|comment|))
         (mentioned (remove-duplicates (all-mentions-from (getf comment :|body|))
                                       :test #'string=))
         (mentioned-slack-ids (to-slack-user-id mentioned)))
    (when (and (string= action "created") mentioned-slack-ids)
      (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                        (generate-message "commented" "issue comment" (getf issue :|title|)
                                          (getf comment :|html_url|)
                                          (getf comment :|body|))
                        mentioned-slack-ids))
    "handled issue-comment"))

(defun handle-pull-request (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (action (getf payload :|action|))
         (pr (getf payload :|pull_request|))
         (assignee (getf payload :|assignee|))
         (mentioned (remove-duplicates (all-mentions-from (getf pr :|body|))
                                       :test #'string=))
         (mentioned-slack-ids (to-slack-user-id mentioned)))
    (cond ((and (string= action "assigned") assignee)
           (let ((assignee (to-slack-user-id (list (getf assignee :|login|)))))
             (when assignee
               (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                                 (generate-message "assigned" "Pull-Request"
                                                   (getf pr :|title|) (getf pr :|html_url|) (getf pr :|body|))
                                 assignee))))
          ((and (string= action "review_requested") (getf pr :|requested_reviewers|))
           (let ((reviewers (to-slack-user-id (mapcar (lambda (rev) (getf rev :|login|))
                                                      (getf pr :|requested_reviewers|)))))
             (print pr)
               (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                                 (generate-message "assigned" "Pull-Request"
                                                   (getf pr :|title|) (getf pr :|html_url|) (getf pr :|body|))
                                 reviewers)))
          ((and (string= action "opened") mentioned-slack-ids)
           (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                             (generate-message "commented" "Pull-Request"
                                               (getf pr :|title|) (getf pr :|html_url|) (getf pr :|body|))
                             mentioned-slack-ids)))
    "handled pull-request"))

(defun handle-pull-request-review (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (action (getf payload :|action|))
         (review (getf payload :|review|)))
    (when (string= action "submitted")
      (let* ((pr (getf payload :|pull_request|))
             (mentioned (remove-duplicates (all-mentions-from (getf review :|body|))
                                           :test #'string=))
             (mentioned-slack-ids (to-slack-user-id mentioned)))
        (when mentioned-slack-ids
          (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                            (generate-message "mentioned" "PR Review"
                                              (getf pr :|title|) (getf review :|html_url|) (getf review :|body|))
                            mentioned-slack-ids))
        "handled pull-request review"))))

(defun handle-pull-request-review-comment (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (action (getf payload :|action|)))
    (print payload)
    (when (string= action "created")
      (let* ((pr (getf payload :|pull_request|))
             (comment (getf payload :|comment|))
             (mentioned (remove-duplicates (all-mentions-from (getf comment :|body|))
                                           :test #'string=))
             (mentioned-slack-ids (to-slack-user-id mentioned)))
        (when mentioned-slack-ids
          (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                            (generate-message "mentioned" "PR Review comment"
                                              (getf pr :|title|) (getf comment :|html_url|) (getf comment :|body|))
                            mentioned-slack-ids))
        "handled pull-request review comment"))))

(defun webhook (env)
  (let ((event-type (gethash "x-github-event"
                             (getf (lack.request:request-env ningle:*request*) :headers))))
    (format t "event-type: ~a~%" event-type)
    (when (stringp event-type)
      (cond ((string= event-type "ping") "hello GitHub!")
        ((string= event-type "issues") (handle-issues env))
        ((string= event-type "pull_request") (handle-pull-request env))
        ((string= event-type "pull_request_review") (handle-pull-request-review env))
        ((string= event-type "pull_request_review_comment") (handle-pull-request-review-comment env))
        ((string= event-type "issue_comment") (handle-issue-comment env))
        (t (format t "do nothing anymore~%"))))))
