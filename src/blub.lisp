(in-package :cl-user)

(defpackage delisp.blub
  (:use :cl :delisp.printer)
  (:export #:blub #:blub-toplevel))
(in-package :delisp.blub)

;; a generic c-like (static types + imperative) language
(defmacro blub-toplevel (&rest code)
  `(mapcar #'gen-statement ',code))

(defun blub (statements)
  (mapcar #'gen-statement statements))

(defun gen-statements (statements)
  (mapcar #'gen-statement statements))

(defun binop-p (op)
  (or (symbol= op :+) (symbol= op :-)
      (symbol= op :*) (symbol= op :/) 
      (symbol= op :%) (symbol= op :==)
      (symbol= op :<) (symbol= op :<=)
      (symbol= op :>) (symbol= op :>=)
      (symbol= op :&&) (symbol= op :||)
      (symbol= op :>>) (symbol= op :<<)
      (symbol= op (make-keyword "&")) (symbol= op (make-keyword "|"))
      (symbol= op (make-keyword "^")) (symbol= op :!=)))

(defun gen-expr (expr &optional (parens nil))
  (if (atom expr)
    (cond ((stringp expr) (emit (format nil "\"~a\"" expr))) 
          ((floatp expr) (emit (format nil "~f" expr)))
          ((null expr))
          (t (emit (format nil "~a" expr))))
    (let ((hd (car expr)))
      (if parens (emit "("))
      (cond
        ;; dot operator - symbol=uivalent to .
        ((symbol= 'dot! hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ".")))
        ((symbol= 'arrow! hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit "->")))

        ((symbol= 'cast! hd)
         (emit "(")
         (gen-expr (cadr expr) t)
         (emit ")")
         (gen-expr (caddr expr) t))

        ;; not
        ((symbol= '! hd)
         (emit "!")
         (gen-expr (cadr expr) t))

        ;; pointer stuff
        ;; note that you can also just put * or & in front of a name
        ;; and it will remain a valid symbol (thanks common lisp!)
        ((symbol= 'ptr! hd)
         (emit "*")
         (gen-expr (cadr expr) t))

        ((symbol= 'addr! hd)
         (emit "&")
         (gen-expr (cadr expr) t))

        ;; indexing (elt ssymbol= idx)
        ((symbol= 'elt! hd)
         (gen-expr (cadr expr))
         (emit "[")
         (gen-expr (caddr expr))
         (emit "]"))

        ;; Binary operator (with parentheses to enforce precedence)
        ((binop-p hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons) t)
               when (cdr cons) do
               (emit (format nil " ~a " (cond
                                          ((symbol= hd :||) "||")
                                          (t hd))))))
        ;; Function call
        (t (gen-expr hd)
           (emit "(")
           (loop for cons on (cdr expr)
                 do (gen-expr (car cons))
                 when (cdr cons) do (emit ", "))
           (emit ")")))
      (if parens (emit ")")))))

(defun gen-statement (statement &optional (semicolon t) (newline t))
  (if (atom statement)
    (emit "~S" statement)
    (let ((hd (car statement)))
      (cond
        ;; (if <cond> <body>*)
        ((symbol= 'if hd)
         (emit "if (")
         (gen-expr (cadr statement))
         (emit ") {" :newline :indent)
         (gen-statements (cddr statement))
         (emit :dedent "}")
         (if newline (emit :newline)))

        ((symbol= 'cond hd)
         (gen-statement (cons 'if (cadr statement)) t nil)
         (loop for cons on (cddr statement)
               do (if (symbol= 'else (caar cons))
                      (progn
                        (emit " else { " :newline :indent)
                        (gen-statements (cdar cons))
                        (emit :dedent "}")
                        (return))
                      (progn
                        (emit " else if (")
                        (gen-expr (caar cons) nil)
                        (emit ") {" :newline :indent)
                        (gen-statements (cdar cons))
                        (emit :dedent "}"))))
        (if newline (emit :newline)))
        ;; (for ((init) (cond) (step)) body*)
        ((symbol= 'for hd)
         (emit "for (")
         (gen-statement (caadr statement) t nil)
         (gen-expr (cadadr statement))
         (emit "; ")
         (gen-statement (car (cddadr statement)) nil nil)
         (emit ") {" :newline :indent)
         (gen-statements (cddr statement))
         (emit :dedent "}" :newline))
        ;; while
        ((symbol= 'while hd)
         (emit "while (")
         (gen-expr (cadr statement))
         (emit ") {" :newline :indent)
         (gen-statements (cddr statement))
         (emit :dedent "}" :newline))
        ;; assignment
        ((symbol= 'set! hd)
         (gen-expr (cadr statement))
         (emit " = ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit :newline)))

        ;; declare w/ a type and assignment in same statement
        ((symbol= 'dset! hd)
         (gen-expr (cadr statement))
         (emit " ")
         (gen-expr (caddr statement))
         (emit " = ")
         (gen-expr (cadddr statement))
         (if semicolon (emit ";"))
         (if newline (emit :newline)))

        ;; +=, -=, *=, /=, and //=
        ((symbol= 'inc! hd)
         (gen-expr (cadr statement))
         (emit " += ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit :newline)))

        ((symbol= 'dec! hd)
         (gen-expr (cadr statement))
         (emit " -= ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit :newline)))

        ((symbol= 'mul! hd)
         (gen-expr (cadr statement))
         (emit " *= ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit :newline)))

        ((symbol= 'div! hd)
         (gen-expr (cadr statement))
         (emit " /= ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit :newline)))

        ;; return
        ((symbol= 'return hd)
         (emit "return ")
         (gen-expr (cadr statement))
         (emit ";" :newline))

        ;; declare variable/type
        ((symbol= 'declare hd)
         (loop for cons on (cdr statement)
               do (emit (format nil "~a" (car cons)))
               when (cdr cons) do (emit " "))
         (if semicolon (emit ";"))
         (if newline (emit :newline)))
        ;; preprocesor macros
        ((symbol= '@declare hd)
         (emit "#")
         (loop for cons on (cdr statement)
               do (emit (format nil "~a" (car cons)))
               when (cdr cons) do (emit " "))
         (if newline (emit :newline)))
        ;; escape arbitrary strings
        ((symbol= '@escape hd)
         (mapcar #'(lambda (x) (emit x :newline)) (cdr statement)))
        ;; (def return-type function-name ((type argname)*) body*)
        ((symbol= 'def hd)
         (gen-function (cdr statement)))
        ;; shorthand function for common return types
        ((common-return-p hd)
         (gen-function statement))
        ;; Assume to be an expression
        (t
         (gen-expr statement)
         (emit ";" :newline))))))

(defun common-return-p (c)
  (or (symbol= c 'float) (symbol= c 'vec2)
      (symbol= c 'vec3) (symbol= c 'vec4)
      (symbol= c 'void) (symbol= c 'int)))

;; (return-type function-name ((type argname)*) body*)
(defun gen-function (statement)
  (emit (format nil "~a ~a(" (car statement) (cadr statement)))
  (loop for cons on (caddr statement)
        do (emit (format nil "~a ~a" (caar cons) (cadar cons)))
        when (cdr cons) do (emit ", "))
  (emit ") {" :newline :indent)
  (gen-statements (cdddr statement))
  (emit :dedent "}" :newline))
