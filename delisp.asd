(defsystem "delisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("unix-opts")
  :components ((:module "src"
                :components
                ((:file "printer")
                 (:file "symbols")
                 (:file "python" :depends-on ("printer" "symbols"))
                 (:file "blub" :depends-on ("printer" "symbols"))
                 (:file "main" :depends-on ("python" "blub")))))
  :description ""
  :build-operation "program-op"
  :build-pathname "bin/delisp"
  :entry-point (symbol-call :delisp :main)
  :in-order-to ((test-op (test-op "delisp/tests"))))

(defsystem "delisp/tests"
  :author ""
  :license ""
  :depends-on ("delisp" "cl-ppcre")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "python"))))
  :description "Test system for delisp")
