#!sbcl --script
(require :asdf)
(asdf:load-system :delisp)
(setf (readtable-case *readtable*) :invert)

(delisp:with-delisp
  (delisp:emit
    `((if (< 10 20) (print "hi"))

      (for x in (range 10)
           (print x))

      (while (not (not True))
             (print (+ "asdf" "fdas")))

      (def hello (name)
           (print "Hello," name)
           (print (+ 1 1)))

      (set! x 2)
      (set! y 10)

      (set! (tuple! x y) (tuple! y x)))))
