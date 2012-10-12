(in-package :cl-user)
(asdf:defsystem #:genheap-test
  :author "Ingvar Mattsson"
  :license "APL"
  :version "1.1"
  :depends-on ("fiveam")
  :components ((:file "test-package")
	       (:file "test-globals" :depends-on ("test-package"))
	       (:file "tests" :depends-on ("test-package" "test-globals"))))

