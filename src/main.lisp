(defpackage delisp
  (:use :cl)
  (:import-from #:delisp.python #:python)
  (:import-from #:delisp.blub #:blub)
  (:import-from #:delisp.printer #:*output* #:*indent* #:*printer* #:printer)
  ;; API and configurable parameters
  (:export
    #:*lang*
    #:*transpilers*
    #:*output*
    #:*indent*
    #:emit
    #:with-delisp
    #:printer))
(in-package :delisp)

(defvar *lang* nil)
(defvar *indent* 4)

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defmacro second-value (multiple-values)
  `(multiple-value-bind (first second) ,multiple-values
    (declare (ignore first))
    second))

(defvar *transpilers* (make-hash-table))
(setf (gethash :py *transpilers*) #'python)
(setf (gethash :c *transpilers*) #'blub)
(setf (gethash :h *transpilers*) #'blub)

(defun emit (expr)
  (let ((transpiler (gethash *lang* *transpilers*)))
    (if transpiler
      (funcall transpiler expr)
      (error "Invalid language: ~s~%" *lang*))))

;; Establishes a preferable environment for delisp-ed code to be emited
;; Utilizes the current filename to determine the filename code should be emitted to
(defmacro with-delisp (&rest code)
  `(let* ((filename (file-namestring *load-truename*))
          (output-filename (uiop:split-name-type filename))
          (ext (second-value (uiop:split-name-type output-filename))))
     (with-open-file (fout output-filename :direction :output :if-exists :supersede)
       (let ((*output* fout)
             (*lang* (make-keyword ext))
             (*printer* (printer *indent*)))
         ,@code))))

;; TODO: refactor python/blub as a list of rules (cons check-if-matches generate-code)
;; TODO: add error handling
;; TODO: allow defining utility functions accessible in every X.Y.lisp file (and sort out environment/package problems)
;; TODO: write custom parser for better handling of case-sensitivity and better errors (ex: traceback, line in which error occurs)
;; (defun main ()
;;   (multiple-value-bind (options free-args) (opts:get-opts)
;;     (declare (ignore options))
;;     (when (null free-args)
;;       (format t "Usage: delisp <file.ext.lisp>~%")
;;       (return-from main nil))
;;     (let* ((filename (car free-args))
;;            (output-filename (uiop:split-name-type filename))
;;            (ext (second-value (uiop:split-name-type output-filename)))
;;            (transpiler (dispatch ext)))
;;       (with-open-file (fout output-filename :direction :output :if-exists :supersede)
;;         (let ((*output* fout)
;;               (lisp-code nil))
;;           (setf (readtable-case *readtable*) :invert)
;;           ;; unfortunately with-safe-io-syntax seems to do weird things to the readtable, so screw safety I guess
;;           ;; (setf lisp-code (uiop:with-safe-io-syntax (:package :delisp.symbols) (uiop:read-file-forms filename)))
;;           (setf lisp-code (uiop:read-file-forms filename))
;;           (funcall transpiler lisp-code))))))
