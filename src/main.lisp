(defpackage langs
  (:use :cl :langs.blub :langs.python :langs.printer)
  (:export
    #:blub
    #:python
    #:*gen-output*
    ))
(in-package :langs)
