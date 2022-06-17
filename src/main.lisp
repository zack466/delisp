(defpackage delisp
  (:use :cl :delisp.blub :delisp.python :delisp.printer)
  (:export
    #:blub
    #:python
    #:*gen-output*
    #:main))
(in-package :delisp)

(defun dispatch (file-extension)
  (cond ((string= file-extension "frag") #'blub)
        ((string= file-extension "c") #'blub)
        ((string= file-extension "h") #'blub)
        ((string= file-extension "py") #'python)
        (t (error "Unrecognized file extension"))))

(defmacro second-value (multiple-values)
  `(multiple-value-bind (first second) ,multiple-values
    (declare (ignore first))
    second))

;; TODO: add error handling
;; TODO: make each lang more modular using some sort of meta-language
;; TODO: allow defining utility functions accessible in every X.Y.lisp file (and sort out environment/package problems)
;; TODO: export language symbols so we can use eq instead of comparing the string value of symbols
(defun main ()
  (multiple-value-bind (options free-args) (opts:get-opts)
    (declare (ignore options))
    (when (null free-args)
      (format t "Usage: delisp <file.ext.lisp>~%")
      (return-from main nil))
    (let* ((filename (car free-args))
           (output-filename (uiop:split-name-type filename))
           (ext (second-value (uiop:split-name-type output-filename)))
           (transpiler (dispatch ext)))
      (with-open-file (fout output-filename :direction :output :if-exists :supersede)
        (let ((*gen-output* fout))
          ;; (in-package :delisp.symbols) ;; read in with correct symbols
          (setf (readtable-case *readtable*) :invert)
          (setf lisp-code (uiop:with-safe-io-syntax (:package :delisp.symbols) (uiop:read-file-forms filename)))
          (in-package :cl-user) ;; execute in user environment, not :delisp.symbols
          (funcall transpiler lisp-code))))))
