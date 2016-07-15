#|
  This file is a part of scheme-impl project.
  Copyright (c) 2016 cl-yaho (s1200191@gmail.com)
|#

(in-package :cl-user)
(defpackage scheme-impl-test-asd
  (:use :cl :asdf))
(in-package :scheme-impl-test-asd)

(defsystem scheme-impl-test
  :author "cl-yaho"
  :license "LLGPL"
  :depends-on (:scheme-impl
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "scheme-impl"))))
  :description "Test system for scheme-impl"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
