(defsystem :niko
  :class :package-inferred-system
  :version "1.2.2"
  :author "TANAKA Shinichi"
  :license "LLGPL"
  :description "Niko is a Slack bot; Niko tells you are mentioned on GitHub"
  :depends-on ("uiop"
               "asdf"

               "cffi"
               "woo"
               "hunchentoot"
               "clack"
               "clack-handler-hunchentoot"
               "ningle"
               "lsx"

               "cl-postgres"
               "cl-dbi"
               "dbd-postgres"
               "mito"

               "cl-redis"

               "dexador"
               "jonathan"

               "niko/main"))

(register-system-packages "cl-redis" '(:redis))
