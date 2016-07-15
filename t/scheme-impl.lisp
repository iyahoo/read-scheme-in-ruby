(in-package :cl21-user)
(defpackage scheme-impl-test
  (:use :cl21
        :scheme-impl
        :prove)
  (:import-from :scheme-impl
                :si/eval))
(in-package :scheme-impl-test)

;; NOTE: To run this test file, execute `(asdf:test-system :scheme-impl)' in your Lisp.

(plan nil)

(subtest "lookup-primitive-fun & apply-primitive-fun"
  (is (apply-primitive-fun (lookup-primitive-fun :+) '(1 2 3)) 6)
  (is (apply-primitive-fun (lookup-primitive-fun :*) '(2 2 2 2 2)) 32)
  (is-error (apply-primitive-fun (lookup-primitive-fun :car) '(1 2 3)) 'simple-error))

(subtest "si/eval"
  (is (si/eval "3") 3)
  (is (si/eval "3.2") 3.2)
  (is (si/eval "100/20") 5)
  (is (si/eval "(:+ 1 2)") 3))

(finalize)
