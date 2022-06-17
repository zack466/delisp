;; Define symbols here so comparisons can be done with eq instead of (string= (string <symbol>))

(in-package :cl-user)

(defpackage delisp.symbols
  (:use :cl)
  (:export
    #:+
    #:-
    #:*
    #:**
    #:/
    #://
    #:%
    #:<
    #:>
    #:>=
    #:<=
    #:==
    #:or
    #:and
    #:dot!
    #:in
    #:not
    #:tuple!
    #:list!
    #:elt!
    #:not
    #:|,lisp|
    #:|,@lisp|
    #:if
    #:cond
    #:for
    #:while
    #:set!
    #:dset!
    #:declare
    #:|#declare|
    #:|#define|
    #:|#ifdef|
    #:inc!
    #:dec!
    #:mul!
    #:div!
    #:idiv!
    #:|#escape|
    #:|#lisp|
    #:return
    #:def
    #:&&
    #:||
    #:!
    ))

;; (asdf:load-system :delisp)
(in-package :delisp.symbols)
