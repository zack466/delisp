(defsystem "langs"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("named-readtables" "unix-opts")
  :components ((:module "src"
                :components
                ( (:file "printer")
                 (:file "python" :depends-on ("printer"))
                 (:file "blub" :depends-on ("printer"))
                 (:file "main" :depends-on ("python" "blub")))))
  :description ""
  :build-operation "program-op"
  :build-pathname "bin/lisp"
  :entry-point (symbol-call :langs :main)
  :in-order-to ((test-op (test-op "langs/tests"))))

(defsystem "langs/tests"
  :author ""
  :license ""
  :depends-on ("langs"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for langs"
  :perform (test-op (op c) (symbol-call :rove :run c)))
