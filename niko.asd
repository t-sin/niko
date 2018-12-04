(defsystem :niko
  :class :package-inferred-system
  :version "0.5.0"
  :author "TANAKA Shinichi"
  :license "LLGPL"
  :description "Niko is a Slack bot; Niko tells you are mentioned on GitHub"
  :depends-on ("uiop"

               "clack"
               "ningle"
               "lsx"

               "mito"

               "niko/main"))