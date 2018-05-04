(defsystem "niko"
  :class :package-inferred-system
  :version "0.1.0"
  :author "TANAKA Shinichi"
  :license ""
  :description ""
  :depends-on ("dexador"
               "jonathan"
               "niko/boot"
               "cl-syntax-annot")
  :in-order-to ((test-op (test-op "niko/tests"))))

(register-system-packages "niko/boot" '(#:niko))
