(in-package :cl-user)

(defpackage delisp.python
  (:use :cl :named-readtables :delisp.printer)
  (:export #:python))

;; (asdf:load-system :delisp)
(in-package :delisp.python)

(defun python (code)
  (mapcar #'gen-statement code))

(defun gen-statements (statements)
  (mapcar #'gen-statement statements))

;; Binary operators
;; Be careful, chaining might not always work as intended
(defun binop-p (op)
  (or (symbol= op '+) (symbol= op '-)
      (symbol= op '*) (symbol= op '**)
      (symbol= op '/) (symbol= op '//)
      (symbol= op '%) (symbol= op '==)
      (symbol= op '<) (symbol= op '<=)
      (symbol= op '>) (symbol= op '>=)
      (symbol= op 'or) (symbol= op 'and)))

(defun gen-expr (expr &optional (parens t))
  (if (atom expr)
    (emit (format nil "~S" expr))
    (let ((hd (car expr)))
      (cond
        ;; dot operator - symbol=uivalent to .
        ((symbol= '|dot!| hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ".")))
        ;; in
        ((symbol= '|in| hd)
         (if parens (emit "("))
         (gen-expr (cadr expr))
         (emit " in ")
         (gen-expr (caddr expr))
         (if parens (emit ")")))

        ;; not
        ((symbol= '|not| hd)
         (if parens (emit "("))
         (emit "not ")
         (gen-expr (cadr expr))
         (if parens (emit ")")))
        ;; tuple
        ((symbol= '|tuple| hd)
         (emit "(")
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ", "))
         (emit ")"))
        ;; list
        ((symbol= '|list| hd)
         (emit "[")
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ", "))
         (emit "]"))
        ;; indexing (elt arr idx)
        ;; For slicing, just use the slice function explicitly
        ((symbol= '|elt| hd)
         (gen-expr (cadr expr))
         (emit "[")
         (gen-expr (caddr expr))
         (emit "]"))
        ;; arbitrary lisp code
        ((symbol= '|,lisp| hd)
         (gen-expr (eval (cons 'progn (cdr expr)))))
        ;; TODO: lambda
        ;; Binary operator (with parentheses to enforce precedence)
        ((binop-p hd)
         (if parens (emit "("))
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit (format nil " ~a " hd)))
         (if parens (emit ")")))
        ;; Function call
        (t
          (gen-expr hd)
          (emit "(")
          (loop for cons on (cdr expr)
                do (gen-expr (car cons) nil)
                when (cdr cons) do (emit ", "))
          (emit ")"))))))

(defun gen-statement (statement &optional (newline t))
  (if (atom statement)
    (emit "~S" statement)
    (let ((hd (car statement)))
      (cond
        ;; (if <cond> <body>*)
        ((symbol= '|if| hd)
         (emit "if ")
         (gen-expr (cadr statement) nil)
         (emit ":" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent)
         (if newline (emit 'newline)))
        ;; chained if/else
        ;; (cond (test body*)*)
        ((symbol= '|cond| hd)
         (gen-statement (cons '|if| (cadr statement)) nil)
         (loop for cons on (cddr statement)
               do (if (or (symbol= '|else| (caar cons))
                          (symbol= '|True| (caar cons)))
                      (progn
                        (emit "else: " 'newline 'indent)
                        (gen-statements (cdar cons))
                        (emit 'dedent)
                        (return))
                      (progn
                        (emit "elif ")
                        (gen-expr (caar cons) nil)
                        (emit ":" 'newline 'indent)
                        (gen-statements (cdar cons))
                        (emit 'dedent)))))
        ;; for
        ((symbol= '|for| hd)
         (emit "for ")
         (gen-expr (cadr statement) nil)
         (emit " in ")
         (assert (symbol= '|in| (caddr statement)))
         (gen-expr (cadddr statement) nil)
         (emit ":" 'newline 'indent)
         (gen-statements (cddddr statement))
         (emit 'dedent 'newline))
        ;; while
        ((symbol= '|while| hd)
         (emit "while ")
         (gen-expr (cadr statement) nil)
         (emit ":" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent 'newline))
        ;; assignment
        ((symbol= '|set!| hd)
         (gen-expr (cadr statement) nil)
         (emit " = ")
         (gen-expr (caddr statement) nil)
         (emit 'newline))
        ;; +=, -=, *=, /=, and //=
        ((symbol= '|inc!| hd)
         (gen-expr (cadr statement) nil)
         (emit " += ")
         (gen-expr (caddr statement) nil)
         (emit 'newline))
        ((symbol= '|dec!| hd)
         (gen-expr (cadr statement) nil)
         (emit " -= ")
         (gen-expr (caddr statement) nil)
         (emit 'newline))
        ((symbol= '|mul!| hd)
         (gen-expr (cadr statement) nil)
         (emit " *= ")
         (gen-expr (caddr statement) nil)
         (emit 'newline))
        ((symbol= '|div!| hd)
         (gen-expr (cadr statement) nil)
         (emit " /= ")
         (gen-expr (caddr statement) nil)
         (emit 'newline))
        ((symbol= '|idiv!| hd)
         (gen-expr (cadr statement) nil)
         (emit " //= ")
         (gen-expr (caddr statement) nil)
         (emit 'newline))
        ;; escape arbitrary strings
        ((symbol= '|#escape| hd)
         (mapcar #'(lambda (x) (emit x 'newline)) (cdr statement)))

        ;; return
        ((symbol= '|return| hd)
         (emit "return ")
         (gen-expr (cadr statement) nil)
         (emit 'newline))
        ;; arbitrary lisp code
        ((symbol= '|#lisp| hd)
         (mapcar #'eval (cdr statement)))
        ((symbol= '|,lisp| hd)
         (gen-statement (eval (cons 'progn (cdr statement)))))
        ((symbol= '|,@lisp| hd)
         (gen-statements (eval (cons 'progn (cdr statement)))))
        ;; declare (use for imports and stuff)
        ((symbol= '|declare| hd)
         (loop for cons on (cdr statement)
               do (emit (format nil "~a" (car cons)))
               when (cdr cons) do (emit " ")))

        ;; (def fname (args) body*)
        ((symbol= '|def| hd)
         (emit (format nil "def ~a(" (cadr statement)))
         (loop for cons on (caddr statement)
               do (emit (format nil "~a" (car cons)))
               when (cdr cons) do (emit ", "))
         (emit "):" 'newline 'indent)
         (gen-statements (cdddr statement))
         (emit 'dedent 'newline))
        ;; TODO: class
        ;; Assume to be an expression
        (t
          (gen-expr statement nil)
          (emit 'newline))))))

;; Limitations:
;; - backslash requires escaping in string literals
;; - excessive parentheses (just use a formatter)
;; - arbitrary lisp code must be written in MODERN CASE

;; Preserve case sensitivity in the world of Python
;; (in-readtable :modern)

;; (PYTHON
;;   `(print (+ 1 (* 3 4) 3) 10)
;;   `(if (< 10 20) (print "hi"))
;;   `(for x in (range 10)
;;         (print x))
;;   `(while (not (not True))
;;           (print (+ "asdf" "fdas")))
;;   `(def hello (name x)
;;         (print "Hello," name x)
;;         (print (+ 1 1)))
;;   `(set! x 2)
;;   `(set! y 10)
;;   `(set! (tuple x y) (tuple y x))
;;   `(map int (dot! (input) (split)))
;;   `((elt fns 3) 10))

;; exit case sensitivity
;; (IN-READTABLE NIL)
