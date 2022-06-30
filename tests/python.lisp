(defpackage delisp/tests/python
  (:use :cl
        :delisp
        :delisp.symbols))
(in-package :delisp/tests/python)

(defmacro test-transpiler (transpiler test-name input satisfy)
  `(let ((*gen-output* (make-string-output-stream))
         (lisp-code nil)
         (input (make-string-input-stream ,input)))
     (in-package :delisp.symbols)
     (setf (readtable-case *readtable*) :invert)
     (setf lisp-code (uiop:slurp-stream-forms input))
     (in-package :cl-user)
     (funcall ,transpiler lisp-code)
     (setf (readtable-case *readtable*) :upcase)
     (if (,satisfy (get-output-stream-string *gen-output*))
         (format t "TEST: ~a PASSED~%" ,test-name)
         (format t "TEST: ~a FAILED~%" ,test-name))))

(test-transpiler
  #'python "print"
  "(print \"Hello,\" \"world!\")"
  (lambda (output)
    (ppcre:scan "print\\(\"Hello,\",\\s*\"world!\"\\)" output)))

(test-transpiler
  #'python "print case-sensitive"
  "(PrInT \"Hello!\")"
  (lambda (output)
    (ppcre:scan "PrInT" output)))

(test-transpiler
  #'python "if statement"
  "(if True (print \"asdf\"))"
  (lambda (output)
    (ppcre:scan "if True:\\n\\s+print" output)))

(test-transpiler
  #'python "if/else statement"
  "(cond (True (print 1)) (else (print 0)))"
  (lambda (output)
    (ppcre:scan "if True:\\n\\s+print\\(1\\)\\nelse:\\n\\s+print\\(0\\)" output)))

(test-transpiler
  #'python "if/elif/else statement"
  "(cond ((> 1 2) (print 1)) ((> 10 11) (print 2)) (else (print 3)))"
  (lambda (output)
    (ppcre:scan "if 1 > 2:\\s+print\\(1\\)\\s+elif 10 > 11:\\s+print\\(2\\)\\s+else:\\s+print\\(3\\)" output)))
