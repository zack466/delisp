(defpackage delisp/tests/main
  (:use :cl
        :delisp
        :rove))
(in-package :delisp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :delisp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
