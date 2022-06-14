(in-package :cl-user)

(defpackage langs.python
  (:use :cl :named-readtables :langs.printer)
  (:export #:python))

;; (asdf:load-system :langs)
(in-package :langs.python)

(defun python (&rest code)
  (mapcar #'gen-statement code))

(defun gen-statements (statements)
  (mapcar #'gen-statement statements))

;; Binary operators
;; Be careful, chaining might not always work as intended
(defun binop-p (op)
  (or (eq op '+) (eq op '-)
      (eq op '*) (eq op '**)
      (eq op '/) (eq op '//)
      (eq op '%) (eq op '==)
      (eq op '<) (eq op '<=)
      (eq op '>) (eq op '>=)
      (eq op 'or) (eq op 'and)))

(defun gen-expr (expr)
  (if (atom expr)
    (emit (format nil "~S" expr))
    (let ((hd (car expr)))
      (cond
        ;; dot operator - equivalent to .
        ((eq '|dot!| hd)
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ".")))
        ;; in
        ((eq '|in| hd)
         (emit "(")
         (gen-expr (cadr expr))
         (emit " in ")
         (gen-expr (caddr expr))
         (emit ")"))

        ;; not
        ((eq '|not| hd)
         (emit "(not ")
         (gen-expr (cadr expr))
         (emit ")"))
        ;; tuple
        ((eq '|tuple| hd)
         (emit "(")
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ", "))
         (emit ")"))
        ;; list
        ((eq '|list| hd)
         (emit "[")
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit ", "))
         (emit "]"))
        ;; indexing (elt seq idx)
        ;; TODO: add support for ranges
        ((eq '|elt| hd)
         (gen-expr (cadr expr))
         (emit "[")
         (gen-expr (caddr expr))
         (emit "]"))
        ;; TODO: lambda
        ;; Binary operator (with parentheses to enforce precedence)
        ((binop-p hd)
         (emit "(")
         (loop for cons on (cdr expr)
               do (gen-expr (car cons))
               when (cdr cons) do (emit (format nil " ~a " hd)))
         (emit ")"))
        ;; Function call
        (t
          (gen-expr hd)
          (emit "(")
          (loop for cons on (cdr expr)
                do (gen-expr (car cons))
                when (cdr cons) do (emit ", "))
          (emit ")"))))))

(defun gen-statement (statement)
  (if (atom statement)
    (emit "~S" statement)
    (let ((hd (car statement)))
      (cond
        ;; (if <cond> <body>*)
        ;; TODO: add chaining (elif)
        ((eq '|if| hd)
         (emit "if ")
         (gen-expr (cadr statement))
         (emit ":" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent 'newline))
        ;; for
        ((eq '|for| hd)
         (emit "for ")
         (gen-expr (cadr statement))
         (emit " in ")
         (assert (eq '|in| (caddr statement)))
         (gen-expr (cadddr statement))
         (emit ":" 'newline 'indent)
         (gen-statements (cddddr statement))
         (emit 'dedent 'newline))
        ;; while
        ((eq '|while| hd)
         (emit "while ")
         (gen-expr (cadr statement))
         (emit ":" 'newline 'indent)
         (gen-statements (cddr statement))
         (emit 'dedent 'newline))
        ;; assignment
        ((eq '|set!| hd)
         (gen-expr (cadr statement))
         (emit " = ")
         (gen-expr (caddr statement))
         (emit 'newline))
        ;; return
        ((eq '|return| hd)
         (emit "return ")
         (gen-expr (cadr statement))
         (emit 'newline))
        ;; (def fname (args) body*)
        ((eq '|def| hd)
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
          (gen-expr statement)
          (emit 'newline))))))

;; Limitations:
;; - backslash requires escaping in string literals
;; - excessive parentheses (just use a formatter)

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
