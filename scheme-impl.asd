#|
  This file is a part of scheme-impl project.
  Copyright (c) 2016 cl-yaho (s1200191@gmail.com)
|#

#|
  Author: cl-yaho (s1200191@gmail.com)
|#

(in-package :cl-user)
(defpackage scheme-impl-asd
  (:use :cl :asdf))
(in-package :scheme-impl-asd)

(defsystem scheme-impl
  :version "0.1"
  :author "cl-yaho"
  :license "LLGPL"
  :defsystem-depends-on (:cl21)
  :class :cl21-system
  :depends-on (:cl-annot
               :optima
               :anaphora
               :cl-dbc-lclj
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "scheme-impl"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op scheme-impl-test))))
