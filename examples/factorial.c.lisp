#!sbcl --script
(require :asdf)
(asdf:load-system :delisp)
(setf (readtable-case *readtable*) :invert)

;; will emit code to file based on the current filename
(delisp:with-delisp
  (delisp:emit
    `((@declare include <stdio.h>)

      (int factorial ((int x))
        (dset! int acc 1)
        (for ((dset! int i 1) (<= i x) (inc! i 1))
          (mul! acc i))
        (return acc))

      (int main ()
           (printf "%d\\n" (factorial 10))
           (return 0)))))
