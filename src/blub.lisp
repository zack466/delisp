(in-package :cl-user)

(defpackage delisp.blub
  (:use :cl :delisp.printer :delisp.symbols)
  (:export #:blub #:blub-toplevel))

;; (asdf:load-system :delisp)
(in-package :delisp.blub)

;; a generic c-like (static types + imperative) language
(defmacro blub-toplevel (&rest code)
  `(mapcar #'gen-statement ',code))

(defun blub (statements)
  (mapcar #'gen-statement statements))

(defun gen-statements (statements)
  (mapcar #'gen-statement statements))

(defun binop-p (op)
  (or (eq op '+) (eq op '-)
      (eq op '*) (eq op '/) 
      (eq op '%) (eq op '==)
      (eq op '<) (eq op '<=)
      (eq op '>) (eq op '>=)
      (eq op '&&) (eq op '||)))

(defun gen-expr (expr &optional (parens nil))
  (if (atom expr)
    (if (stringp expr)
        (emit (format nil "\"~a\"" expr)) 
        (emit (format nil "~a" expr)))
    (let ((hd (car expr)))
      (if parens (emit "("))
      (cond
        ;; dot operator - equivalent to .
        ((symbol= 'jdot!j hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ".")))
        ;; not
        ((symbol= '! hd)
         (emit "!")
         (gen-expr (cadr expr) t))

        ;; indexing (elt seq idx)
        ((symbol= 'elt! hd)
         (gen-expr (cadr expr))
         (emit "[")
         (gen-expr (caddr expr))
         (emit "]"))

        ((symbol= '|,lisp| hd)
         (gen-expr (eval (cons 'progn (cdr expr)))))

        ;; Binary operator (with parentheses to enforce precedence)
        ((binop-p hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons) t)
               when (cdr cons) do (emit (format nil " ~S " hd))))
        ;; Function call
        (t
          (gen-expr hd)
          (emit "(")
          (loop for cons on (cdr expr)
                do (gen-expr (car cons))
                when (cdr cons) do (emit ", "))
          (emit ")")))
      (if parens (emit ")"))
      )
    )
  )

(defun gen-statement (statement &optional (semicolon t) (newline t))
  (if (atom statement)
    (emit "~S" statement)
    (let ((hd (car statement)))
      (cond
        ;; (if <cond> <body>*)
        ;; TODO: add chaining (elif)
        ((symbol= 'if hd)
         (emit "if (")
         (gen-expr (cadr statement))
         (emit ") {" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent "}" 'newline))

        ;; C-style preprocesor macros
        ((symbol= '|#ifdef| hd)
         (emit "#ifdef ")
         (gen-expr (cadr statement))
         (emit 'newline)
         (gen-statements (cddr statement))
         (emit "#endif" 'newline))

        ((symbol= '|#define| hd)
         (emit "#define ")
         (gen-expr (cadr statement))
         (emit " ")
         (gen-expr (caddr statement))
         (emit 'newline))
        ;; (for ((init) (cond) (step)) body*)
        ((symbol= 'for hd)
         (emit "for (")
         (gen-statement (caadr statement) t nil)
         (gen-expr (cadadr statement))
         (emit "; ")
         (gen-statement (car (cddadr statement)) nil nil)
         (emit ") {" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent "}" 'newline))
        ;; while
        ((symbol= 'while hd)
         (emit "while (")
         (gen-expr (cadr statement))
         (emit ") {" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent "}" 'newline))
        ;; assignment
        ((symbol= 'set! hd)
         (gen-expr (cadr statement))
         (emit " = ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ;; declare w/ a type and assignment in same statement
        ((symbol= 'dset! hd)
         (gen-expr (cadr statement))
         (emit " ")
         (gen-expr (caddr statement))
         (emit " = ")
         (gen-expr (cadddr statement))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ;; +=, -=, *=, /=, and //=
        ((symbol= 'inc! hd)
         (gen-expr (cadr statement))
         (emit " += ")
         (gen-expr (caddr statement))
         (emit ";" 'newline))
        ((symbol= 'dec! hd)
         (gen-expr (cadr statement))
         (emit " -= ")
         (gen-expr (caddr statement))
         (emit ";" 'newline))
        ((symbol= 'mul! hd)
         (gen-expr (cadr statement))
         (emit " *= ")
         (gen-expr (caddr statement))
         (emit ";" 'newline))
        ((symbol= 'div! hd)
         (gen-expr (cadr statement))
         (emit " /= ")
         (gen-expr (caddr statement))
         (emit ";" 'newline))

        ;; return
        ((symbol= 'return hd)
         (emit "return ")
         (gen-expr (cadr statement))
         (emit ";" 'newline))

        ;; declare variable/type
        ((symbol= 'declare hd)
         (loop for cons on (cdr statement)
               do (emit (format nil "~a" (car cons)))
               when (cdr cons) do (emit " "))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ;; useful for preprocesor macros
        ((symbol= '|#declare| hd)
         (loop for cons on (cdr statement)
               do (emit (format nil "~a" (car cons)))
               when (cdr cons) do (emit " "))
         (if newline (emit 'newline)))
        ;; escape arbitrary strings
        ((symbol= '|#escape| hd)
         (mapcar #'(lambda (x) (emit x 'newline)) (cdr statement)))
        ;; (def return-type function-name ((type argname)*) body*)
        ((symbol= 'def hd)
         (gen-function (cdr statement)))
        ;; shorthand function for common return types
        ((common-return-p hd)
         (gen-function statement))
        ;; escape function for arbitrary lisp code
        ((eq '|#lisp| hd)
         (mapcar #'eval (cdr statement)))
        ((eq '|,lisp| hd)
         (gen-statement (eval (cons 'progn (cdr statement)))))
        ((eq '|,@lisp| hd)
         (gen-statements (eval (cons 'progn (cdr statement)))))
        ;; Assume to be an expression
        (t
         (gen-expr statement)
         (emit ";" 'newline))))))

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
  (emit ") {" 'newline 'indent)
  (gen-statements (cdddr statement))
  (emit 'dedent "}" 'newline))

;; (in-readtable :modern)

;; (BLUB
;;   `((declare uniform float x)
;;     (set! x 2)
;;     (dset! vec4 color (vec4 1.0 0.0 1.0 1.0))
;;     (if (&& (! false) (+ 2 4)) (set! x 4) (set! y (- 0 100)))
;;     (void main ((string args) (int x))
;;           (set! x 10)
;;           (if true (set! y 20)))
;;     (float plot ((vec2 st))
;;            (return (smoothstep 0.02 0.0 (abs (- st.x st.y)))))))

;; (IN-READTABLE NIL)
