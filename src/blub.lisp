(in-package :cl-user)

(defpackage langs.blub
  (:use :cl :named-readtables :langs.printer)
  (:export #:blub #:blub-toplevel))

;; (asdf:load-system :langs)
(in-package :langs.blub)

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
    (emit (format nil "~S" expr))
    (let ((hd (car expr)))
      (if parens (emit "("))
      (cond
        ;; dot operator - equivalent to .
        ((eq '|dot!| hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ".")))
        ;; not
        ((eq '|!| hd)
         (emit "!")
         (gen-expr (cadr expr) t))

        ;; indexing (elt seq idx)
        ((eq '|elt| hd)
         (gen-expr (cadr expr))
         (emit "[")
         (gen-expr (caddr expr))
         (emit "]"))

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

(defun gen-statement (statement)
  (if (atom statement)
    (emit "~S" statement)
    (let ((hd (car statement)))
      (cond
        ;; (if <cond> <body>*)
        ;; TODO: add chaining (elif)
        ((eq '|if| hd)
         (emit "if (")
         (gen-expr (cadr statement))
         (emit ") {" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent "}" 'newline))
        ;; for
        ;; ((eq '|for| hd)
        ;;  (emit "for ")
        ;;  (gen-expr (cadr statement))
        ;;  (emit " in ")
        ;;  (assert (eq '|in| (caddr statement)))
        ;;  (gen-expr (cadddr statement))
        ;;  (emit ":" 'newline 'indent)
        ;;  (gen-statements (cddddr statement))
        ;;  (emit 'dedent 'newline))
        ;; while
        ((eq '|while| hd)
         (emit "while (")
         (gen-expr (cadr statement))
         (emit ") {" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent "}" 'newline))
        ;; assignment
        ((eq '|set!| hd)
         (gen-expr (cadr statement))
         (emit " = ")
         (gen-expr (caddr statement))
         (emit ";" 'newline))

        ;; declare and assignment in same statement
        ((eq '|dset!| hd)
         (emit (format nil "~a " (cadr statement)))
         (gen-expr (caddr statement))
         (emit " = ")
         (gen-expr (cadddr statement))
         (emit  ";" 'newline))

        ;; return
        ((eq '|return| hd)
         (emit "return ")
         (gen-expr (cadr statement))
         (emit ";" 'newline))

        ;; declare variable/type
        ((eq '|declare| hd)
         (loop for cons on (cdr statement)
               do (emit (format nil "~a" (car cons)))
               when (cdr cons) do (emit " "))
         (emit ";" 'newline))
        ;; (def return-type function-name ((type argname)*) body*)
        ((eq '|def| hd)
         (gen-function (cdr statement)))
        ;; shorthand function for common return types
        ((common-return-p hd)
         (gen-function statement))
        ;; escape function for arbitrary lisp code
        ((eq '|lisp| hd)
         (apply #'eval (cdr statement)))
        ;; Assume to be an expression
        (t
         (gen-expr statement)
         (emit ";" 'newline))))))

(defun common-return-p (c)
  (or (eq c '|float|) (eq c '|vec2|)
      (eq c '|vec3|) (eq c '|vec4|)
      (eq c '|void|)))

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
