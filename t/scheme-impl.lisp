(in-package :cl-user)
(defpackage scheme-impl-test
  (:use :cl
        :scheme-impl
        :prove))
(in-package :scheme-impl-test)

;; NOTE: To run this test file, execute `(asdf:test-system :scheme-impl)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
