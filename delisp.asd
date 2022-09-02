(defsystem "delisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "printer")
                 (:file "python" :depends-on ("printer"))
                 (:file "blub" :depends-on ("printer"))
                 (:file "main" :depends-on ("python" "blub" "printer")))))
  :description ""
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
