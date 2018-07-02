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

(defun handle-issues (env)
  (format t "check if assined~%~s~%" env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (issue (getf payload :|issue|))
         (mentioned (remove-duplicates (all-mentions-from (getf issue :|body|))
                                       :test #'string=))
         (mentioned-slack-ids (to-slack-user-id mentioned)))
    (when mentioned-slack-ids
      (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                        (format nil "~%: You are mentioned on the issue `~a`~%~a"
                                (getf issue :|title|)
                                (getf issue :|html_url|))
                        mentioned-slack-ids))
    "handled issue"))

(defun handle-pull-request (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (pr (getf payload :|pull_request|))
         (mentioned (remove-duplicates (all-mentions-from (getf pr :|body|))
                                       :test #'string=))
         (mentioned-slack-ids (to-slack-user-id mentioned)))
    (when mentioned-slack-ids
      (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                        (format nil "~%: You are mentioned on the PR `~a`~%~a"
                                (getf pr :|title|)
                                (getf pr :|html_url|))
                        mentioned-slack-ids))
  "handled pull-request"))

(defun handle-issue-comment (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (issue (getf payload :|issue|))
         (comment (getf payload :|comment|))
         (mentioned (remove-duplicates (all-mentions-from (getf comment :|body|))
                                       :test #'string=))
         (mentioned-slack-ids (to-slack-user-id mentioned)))
    (when mentioned-slack-ids
      (api/post-message (api/channel-id (uiop:getenv "SLACK_CHANNEL"))
                        (format nil "~%: You are mentioned on the issue comment `~a`~%~a"
                                (getf issue :|title|)
                                (getf comment :|html_url|))
                        mentioned-slack-ids))
    "handled issue-comment"))

(defun webhook (env)
  (let ((event-type (gethash "x-github-event"
                             (getf (lack.request:request-env utopian:*request*) :headers))))
    (when (stringp event-type)
      (cond ((string= event-type "ping") "hello GitHub!")
        ((string= event-type "issues") (handle-issues env))
        ((string= event-type "pull_request") (handle-pull-request env))
        ((string= event-type "issue_comment") (handle-issue-comment env))
        (t (format t "do nothing anymore~%"))))))
