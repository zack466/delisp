#!sbcl --script
(require :asdf)
(asdf:load-system :delisp)
(setf (readtable-case *readtable*) :invert)

;; insert here user code
(setf str "Hello, world!\\n")

;; will emit code to file based on the current filename
(delisp:with-delisp
  (delisp:emit
    `((@declare include <stdio.h>)
      (int main ()
           (printf ,str)
           (return 0)))))
