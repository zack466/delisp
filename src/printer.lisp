(in-package :cl-user)

(defpackage delisp.printer
  (:use :cl)
  (:export
    #:*output*
    #:*printer*
    #:printer
    #:emit
    #:symbol=
    #:*indent*
    #:make-keyword
    ))

(in-package :delisp.printer)

;; defaults
(defvar *output* t)

(defun symbol= (a b)
  (if (and (symbolp a) (symbolp b))
      (string= (symbol-name a) (symbol-name b))
      nil))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

;; printing instructions are any of:
;; - a string literal
;; - 'indent
;; - 'dedent
;; - 'newline
(defun printer (indent-size)
  (let ((indent-level 0)
        (need-to-indent nil))
    (lambda (i)
      (cond
        ((stringp i)
         (when need-to-indent
           (format *output* "~a" (make-string (* indent-size indent-level) :initial-element #\Space))
           (setf need-to-indent nil))
         (format *output* "~a" i))
        ((eq :indent i) (incf indent-level))
        ((eq :dedent i) (decf indent-level))
        ((eq :newline i) (format *output* "~%") (setf need-to-indent t))
        (t (error "Unsupported printer instruction: ~S~%" i))))))

(defvar *printer* (printer 4))

(defun emit (&rest ins)
  (mapcar #'(lambda (i) (funcall *printer* i)) ins))
