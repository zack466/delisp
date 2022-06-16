(in-package :cl-user)

(defpackage langs.printer
  (:use :cl)
  (:export
    #:*gen-output*
    #:*printer*
    #:printer
    #:emit
    #:symbol=))

(in-package :langs.printer)

(defvar *gen-output* t)

(defun symbol= (a b)
  (string= (symbol-name a) (symbol-name b)))

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
           (format *gen-output* "~a" (make-string (* indent-size indent-level) :initial-element #\Space))
           (setf need-to-indent nil))
         (format *gen-output* "~a" i))
        ((symbol= 'indent i) (incf indent-level))
        ((symbol= 'dedent i) (decf indent-level))
        ((symbol= 'newline i) (format *gen-output* "~%") (setf need-to-indent t))
        (t (error "Unsupported printer instruction: ~S~%" i))))))

(defvar *printer* (printer 4))

(defun emit (&rest ins)
  (mapcar #'(lambda (i) (funcall *printer* i)) ins))
