(defpackage #:niko/api/github-webhook
  (:use #:cl
        #:string-case)
  (:import-from #:niko/lib/slack
                #:api/channel-id
                #:api/user-ids
                #:api/post-message)
  (:import-from #:jonathan
                #:parse)
  (:export #:webhook))
(in-package #:niko/api/github-webhook)

(defun all-mentions-from (text)
  (mapcar (lambda (s) (subseq s 1))
          (ppcre:all-matches-as-strings "@[^ ]+" text)))


(defun handle-issues (env)
  (format t "check if assined~%~s~%" env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (issue (getf payload :|issue|)))
    (format t "issue title: ~s~%" (getf issue :|title|))
    (format t "issue url: ~s~%" (getf issue :|html_url|))
    (format t "mentioned users: ~s~%"
            (all-mentions-from (getf issue :|body|)))))

(defun handle-pull-request (env)
  (format t "check if assined~%"))

;;; issue comments

(defun handle-issue-comment (env)
  (let* ((payload (parse (cdr (assoc "payload" env :test #'string=))))
         (issue (getf payload :|issue|))
         (comment (getf payload :|comment|)))
;;    (format t "~S~%" payload)
    (let* ((owner (getf (getf issue :|user|) :|login|))
           (mentioned (remove-duplicates (append (list owner)
                                                 (all-mentions-from (getf comment :|body|)))
                                         :test #'string=)))
           (format t "~s~%" (api/user-ids mentioned))
      (api/post-message (api/channel-id "XXXX")
                        (format nil "Commented on the issue `~a`~%~a"
                                (getf issue :|title|)
                                (getf comment :|html_url|))
                        mentioned))))

(defun webhook (env)
  (let ((event-type (gethash "x-github-event"
                             (getf (lack.request:request-env utopian:*request*) :headers))))
    (when (stringp event-type)
      (string-case (event-type)
        ("ping" "hello GitHub!")
        ("issues" (handle-issues env))
        ("pull_request" (handle-pull-request env))
        ("issue_comment" (handle-issue-comment env))
        (t (format t "do nothing anymore~%"))))))
