#!sbcl --script
(require :asdf)
(asdf:load-system :delisp)
(setf (readtable-case *readtable*) :invert)

;; insert here user code
(setf code (loop for i from 1 to 5 collect `(print ,i)))

;; will emit code to file based on the current filename
(delisp:with-delisp
  (delisp:emit code))
