(defpackage #:niko/watcher
  (:use #:cl)
  (:import-from #:niko/lib/github
                #:api/notifications
                #:api/issue
                #:api/issue-comment
                #:api/user)
  (:import-from #:local-time
                #:universal-to-timestamp)
  (:import-from #:cl-date-time-parser
                #:parse-date-time))
(in-package #:niko/watcher)


(defun notifications-with-mentions (notifications)
  (loop
    :for n :in notifications
    :when (member (getf n :|reason|) '("author"  ; 自分がauthorのスレッドにコメントきたから
                                       "comment"  ; 自分がスレッドにコメントしたから
                                       "mention"  ; 明示的なメンションされたから
                                       "assign"  ; 割り当てされたとき
                                       "subscribed")  ; スレッド購読してたから
                  :test #'string=)
    :collect (list (getf n :|subject|)
                   (intern (getf n :|reason|) :keyword))))

(defun parse-subject (subject)
  (let ((issue (multiple-value-bind (match strs)
                   (ppcre:scan-to-strings "/repos/([^/]+)/([^/]+)/issues/(.+$)"
                                          (getf subject :|url|))
                 (when match
                   (let ((owner (aref strs 0))
                         (repo (aref strs 1))
                         (issue-id (aref strs 2)))
                     (list owner repo issue-id)))))
        (pull (multiple-value-bind (match strs)
                  (ppcre:scan-to-strings "/repos/([^/]+)/([^/]+)/pulls/(.+$)"
                                         (getf subject :|url|))
                (when match
                   (let ((owner (aref strs 0))
                         (repo (aref strs 1))
                         (pull-id (aref strs 2)))
                     (list owner repo pull-id)))))
        (comment-id (multiple-value-bind (match strs)
                        (ppcre:scan-to-strings "/repos/[^/]+/[^/]+/issues/comments/(.+$)"
                                               (getf subject :|latest_comment_url|))
                      (when match
                        (aref strs 0)))))
    (values issue pull comment-id)))

(defun retrieve-issue-body (subject)
  (multiple-value-bind (issue pull comment-id)
      (parse-subject subject)
    (when (or issue pull)
      (destructuring-bind (owner repo issue-id) (or issue pull)
        (let ((issue (if comment-id
                         (api/issue-comment owner repo comment-id)
                         (api/issue owner repo issue-id))))
          (values (getf issue :|url|)
                  (getf issue :|body|)))))))

(defun all-mentions-from (text)
  (ppcre:all-matches-as-strings "@[^ ]+" text))

(defun all-mentions (nots)
  (loop
    :for n :in nots
    :nconc (multiple-value-bind (issue-url issue-body)
               (retrieve-issue-body (first n))
              (let ((mentions (all-mentions-from issue-body)))
                (when mentions
                 (list issue-url mentions))))))


(defun watch ()
  (loop
    (multiple-value-bind (response last-modified poll-interval)
        (api/notifications)
      (let ((last-modified (universal-to-timestamp (parse-date-time last-modified)))
            (nots (notifications-with-mentions response)))
        (format t "~s~%" nots)
        (format t "last-modified: ~s" last-modified)
        (sleep poll-interval)))))
