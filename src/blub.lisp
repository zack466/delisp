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
      (eq op '&&) (eq op '||)
      (eq op '>>) (eq op '<<)
      (eq op 'and!) (eq op 'or!)
      (eq op 'xor!) (eq op '!=)))

(defun gen-expr (expr &optional (parens nil))
  (if (atom expr)
      (cond ((stringp expr) (emit (format nil "\"~a\"" expr))) 
            ((floatp expr) (emit (format nil "~f" expr)))
            ((null expr))
            (t (emit (format nil "~a" expr))))
      (let ((hd (car expr)))
      (if parens (emit "("))
      (cond
        ;; dot operator - equivalent to .
        ((eq 'dot! hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ".")))
        ((eq 'arrow! hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit "->")))

        ((eq 'cast! hd)
         (emit "(")
         (gen-expr (cadr expr) t)
         (emit ")")
         (gen-expr (caddr expr) t))
        
        ;; not
        ((eq '! hd)
         (emit "!")
         (gen-expr (cadr expr) t))
        
        ;; pointer stuff
        ;; note that you can also just put * or & in front of a name
        ;; and it will remain a valid symbol (thanks common lisp!)
        ((eq 'ptr! hd)
         (emit "*")
         (gen-expr (cadr expr) t))

        ((eq 'addr! hd)
         (emit "&")
         (gen-expr (cadr expr) t))

        ;; indexing (elt seq idx)
        ((eq 'elt! hd)
         (gen-expr (cadr expr))
         (emit "[")
         (gen-expr (caddr expr))
         (emit "]"))

        ((eq '|,lisp| hd)
         (gen-expr (eval (cons 'progn (cdr expr)))))

        ;; Binary operator (with parentheses to enforce precedence)
        ((binop-p hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons) t)
               when (cdr cons) do
               (emit (format nil " ~a " (cond
                                          ((eq hd '||) "||")
                                          ((eq hd 'and!) "&")
                                          ((eq hd 'or!) "|")
                                          ((eq hd 'xor!) "^")
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
        ((eq 'if hd)
         (emit "if (")
         (gen-expr (cadr statement))
         (emit ") {" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent "}")
         (if newline (emit 'newline)))

        ((eq 'cond hd)
         (gen-statement (cons 'if (cadr statement)) t nil)
         (loop for cons on (cddr statement)
               do (if (eq 'else (caar cons))
                      (progn
                        (emit " else { " 'newline 'indent)
                        (gen-statements (cdar cons))
                        (emit 'dedent "}")
                        (return))
                      (progn
                        (emit " else if (")
                        (gen-expr (caar cons) nil)
                        (emit ") {" 'newline 'indent)
                        (gen-statements (cdar cons))
                        (emit 'dedent "}"))))
        (if newline (emit 'newline)))

        ;; C-style preprocesor macros
        ((eq '|#ifdef| hd)
         (emit "#ifdef ")
         (gen-expr (cadr statement))
         (emit 'newline)
         (gen-statements (cddr statement))
         (emit "#endif" 'newline))

        ((eq '|#define| hd)
         (emit "#define ")
         (gen-expr (cadr statement))
         (emit " ")
         (gen-expr (caddr statement))
         (emit 'newline))
        ;; (for ((init) (cond) (step)) body*)
        ((eq 'for hd)
         (emit "for (")
         (gen-statement (caadr statement) t nil)
         (gen-expr (cadadr statement))
         (emit "; ")
         (gen-statement (car (cddadr statement)) nil nil)
         (emit ") {" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent "}" 'newline))
        ;; while
        ((eq 'while hd)
         (emit "while (")
         (gen-expr (cadr statement))
         (emit ") {" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent "}" 'newline))
        ;; assignment
        ((eq 'set! hd)
         (gen-expr (cadr statement))
         (emit " = ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ;; declare w/ a type and assignment in same statement
        ((eq 'dset! hd)
         (gen-expr (cadr statement))
         (emit " ")
         (gen-expr (caddr statement))
         (emit " = ")
         (gen-expr (cadddr statement))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ;; +=, -=, *=, /=, and //=
        ((eq 'inc! hd)
         (gen-expr (cadr statement))
         (emit " += ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ((eq 'dec! hd)
         (gen-expr (cadr statement))
         (emit " -= ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ((eq 'mul! hd)
         (gen-expr (cadr statement))
         (emit " *= ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ((eq 'div! hd)
         (gen-expr (cadr statement))
         (emit " /= ")
         (gen-expr (caddr statement))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ;; return
        ((eq 'return hd)
         (emit "return ")
         (gen-expr (cadr statement))
         (emit ";" 'newline))

        ;; declare variable/type
        ((eq 'declare hd)
         (loop for cons on (cdr statement)
               do (emit (format nil "~a" (car cons)))
               when (cdr cons) do (emit " "))
         (if semicolon (emit ";"))
         (if newline (emit 'newline)))

        ;; useful for preprocesor macros
        ((eq '|#declare| hd)
         (loop for cons on (cdr statement)
               do (emit (format nil "~a" (car cons)))
               when (cdr cons) do (emit " "))
         (if newline (emit 'newline)))
        ;; escape arbitrary strings
        ((eq '|#escape| hd)
         (mapcar #'(lambda (x) (emit x 'newline)) (cdr statement)))
        ;; (def return-type function-name ((type argname)*) body*)
        ((eq 'def hd)
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
  (or (eq c 'float) (eq c 'vec2)
      (eq c 'vec3) (eq c 'vec4)
      (eq c 'void) (eq c 'int)))

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
