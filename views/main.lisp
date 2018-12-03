(defpackage #:niko/views/main
  (:use #:cl #:lsx)
  (:import-from #:niko/app
                #:defroute)
  (:export #:view-template))
(in-package #:niko/views)

(defroute ("/" :GET)
  (let ((template (read-lsx-file #P"views/template.lsx")))
    (render-object (funcall template
                            :title "Niko - Not-a-cat Slack bot") nil)))
