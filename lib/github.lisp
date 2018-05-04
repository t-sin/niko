(defpackage #:niko/lib/github
  (:use #:cl)
  (:export #:api/notifications
           #:api/issue
           #:api/issue-comment))
(in-package #:niko/lib/github)

;;;; APIs
(defun api/notifications (&optional since all)
  (handler-bind
      ((error
        (lambda (c)
          (format t "~s~%" c)
          (return-from api/notifications))))
    (multiple-value-bind (res status header uri ssl)
        (let ((uri (format nil "~a?~a"
                           "https://api.github.com/notifications"
                           (quri:url-encode-params `(,(if since
                                                          `("since" . ,since)
                                                          '("since" . ""))
                                                      ,(if all
                                                           `("all" . ,all)
                                                           '("all" . "false")))))))
          (dex:get uri
                   :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN"))))))
      (declare (ignore status ssl uri))
      (values
       (jojo:parse res)
       (gethash "last-modified" header)
       (gethash "x-poll-interval" header)))))

(defun api/issue (owner repo id)
  (handler-bind
      ((error
         (lambda (c)
           (format t "~s~%" c))))
    (multiple-value-bind (res status header uri ssl)
        (let ((uri (format nil "~a/repos/~a/~a/issues/~a"
                           "https://api.github.com"
                           owner repo id)))
          (dex:get uri :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN"))))))
      (declare (ignore header status ssl uri))
      (jojo:parse res))))

(defun api/issue-comment (owner repo id)
  (handler-bind
      ((error
         (lambda (c)
           (format t "~s~%" c))))
    (multiple-value-bind (res status header uri ssl)
        (let ((uri (format nil "~a/repos/~a/~a/issues/comments/~a"
                           "https://api.github.com"
                           owner repo id)))
          (dex:get uri :headers `(("Authorization" . ,(format nil "token ~a" (uiop:getenv "GITHUB_TOKEN"))))))
      (declare (ignore header status ssl uri))
      (jojo:parse res))))


;;;; utils
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
        (comment-id (multiple-value-bind (match strs)
                        (ppcre:scan-to-strings "/repos/[^/]+/[^/]+/issues/comments/(.+$)"
                                               (getf subject :|latest_comment_url|))
                      (when match
                        (aref strs 0)))))
    (values issue comment-id)))


(defun retrieve-issue-body (subject)
  (multiple-value-bind (issue comment-id)
      (parse-subject subject)
    (destructuring-bind (owner repo issue-id) issue
      (let ((issue (if comment-id
                       (api/issue-comment owner repo comment-id)
                       (api/issue owner repo issue-id))))
        (values (getf issue :|url|)
                (getf issue :|body|))))))

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
