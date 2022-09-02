(defpackage delisp/tests/py
  (:use :cl :delisp))
(in-package :delisp/tests/py)

(defmacro deftest (test-name language input satisfy)
  (setf (readtable-case *readtable*) :invert)
  `(let ((*output* (make-string-output-stream))
         (*lang* ,language))
     (emit ,input)
     (if (,satisfy (get-output-stream-string *output*))
         (format t "TEST: ~a PASSED~%" ,test-name)
         (format t "TEST: ~a FAILED~%" ,test-name))))

(deftest "print" :py
         '((print "Hello," "world!"))
         (lambda (output)
           (ppcre:scan "print\\(\"Hello,\",\\s*\"world!\"\\)" output)))

(deftest "print case-sensitive" :py 
         '((PrInT "Hello!"))
         (lambda (output)
           (ppcre:scan "PrInT" output)))

(deftest "if statement" :py 
         '((if True (print "asdf")))
         (lambda (output)
           (ppcre:scan "if True:\\n\\s+print" output)))

(deftest "if/else statement" :py 
         '((cond (True (print 1)) (else (print 0))))
         (lambda (output)
           (ppcre:scan "if True:\\n\\s+print\\(1\\)\\nelse:\\n\\s+print\\(0\\)" output)))

(deftest "if/elif/else statement" :py 
         '((cond ((> 1 2) (print 1)) ((> 10 11) (print 2)) (else (print 3))))
         (lambda (output)
           (ppcre:scan "if 1 > 2:\\s+print\\(1\\)\\s+elif 10 > 11:\\s+print\\(2\\)\\s+else:\\s+print\\(3\\)" output)))
