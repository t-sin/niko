(defpackage #:niko/watcher
  (:use #:cl)
  (:import-from #:niko/lib/github
                #:api/notifications
                #:api/issue
                #:api/issue-comment
                #:api/user)
  (:import-from #:local-time
                #:format-timestring
                #:now
                #:timestamp<
                #:universal-to-timestamp)
  (:import-from #:cl-date-time-parser
                #:parse-date-time)
  (:export #:watch
           #:watch-forever))
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


(defun watch (&optional last-modified)
  (multiple-value-bind (response %last-modified poll-interval)
      (if last-modified
          (api/notifications (format-timestring nil last-modified
                                                :format local-time:+rfc-1123-format+))
(defun format-ts (timestamp)
  (format-timestring nil timestamp
                     :format '((:year 4 0) #\- (:month 2 #\0) #\- (:day 2 #\0) #\T
                               (:hour 2 #\0) #\: (:min 2 #\0) #\: (:sec 2 #\0) #\Z)
                     :timezone local-time:+utc-zone+))

          (api/notifications))
    (if (and (not (null %last-modified))
             (setf %last-modified (universal-to-timestamp (parse-date-time %last-modified)))
             (or (null last-modified) (timestamp< last-modified %last-modified)))
        (let* ((nots (notifications-with-mentions response))
               (mentions (all-mentions nots)))
          (format t "~s~%"  mentions)
          (format t ";;; posting...~%")
          (values %last-modified poll-interval))
        (progn
          (format t ";;; nothing to notify~%")
          (values nil poll-interval)))))


(defun watch-forever ()
  (loop
    :with last-checked := (now)
    :do (let ((poll-interval (watch last-checked)))
          (setf last-checked (local-time:timestamp- (now) 1 :minute))
          (sleep poll-interval))))
