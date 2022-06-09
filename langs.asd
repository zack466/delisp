(defsystem "langs"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("alexandria" "named-readtables")
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "python"))))
  :description ""
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
