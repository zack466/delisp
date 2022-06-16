(defpackage langs
  (:use :cl :langs.blub :langs.python :langs.printer :named-readtables)
  (:export
    #:blub
    #:python
    #:*gen-output*
    #:main))
(in-package :langs)

(defun dispatch (file-extension)
  (cond ((string= file-extension "frag") #'blub)
        ((string= file-extension "c") #'blub)
        ((string= file-extension "h") #'blub)
        ((string= file-extension "py") #'python)
        (t (error "Unrecognized file extension"))))

(defmacro second-value (multiple-values)
  `(multiple-value-bind (first second)
       ,multiple-values
     second))

;; TODO: add error handling
;; TODO: make each lang more modular using some sort of meta-language
;; TODO: allow defining utility functions accessible in every X.Y.lisp file (and sort out environment/package problems)
;; TODO: export language symbols so we can use eq instead of comparing the string value of symbols
(defun main ()
  (multiple-value-bind (options free-args) (opts:get-opts)
    (when (null free-args)
      (format t "Usage: delisp <file.ext.lisp>~%")
      (return-from main nil))
    (let* ((filename (car free-args))
           (output-filename (uiop:split-name-type filename))
           (ext (second-value (uiop:split-name-type output-filename)))
           (transpiler (dispatch ext)))

      (in-readtable :modern)
      (SETF LISP-CODE (UIOP:READ-FILE-FORMS FILENAME))

      (WITH-OPEN-FILE (FOUT OUTPUT-FILENAME :DIRECTION :OUTPUT :IF-EXISTS :SUPERSEDE)
        (LET ((*GEN-OUTPUT* FOUT))
          (FUNCALL TRANSPILER LISP-CODE)))
      (IN-READTABLE NIL))))
