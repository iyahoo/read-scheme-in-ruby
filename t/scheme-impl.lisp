(in-package :cl21-user)
(defpackage scheme-impl-test
  (:use :cl21
        :scheme-impl
        :prove))
(in-package :scheme-impl-test)

;; NOTE: To run this test file, execute `(asdf:test-system :scheme-impl)' in your Lisp.

(plan nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variable
;; *global-env* is defiend in :scheme-impl

(defparameter *env* *global-env*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup

(subtest "lookup-var"
  (is (lookup-var :y #H(:x 1 :y 2))
      2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicate

(subtest "letp"
  (ok (letp '(:let ((:x 1) (:y 2)) :x)))
  (ok (not (letp '(:let ())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env

(subtest "extend-env"
  (is (extend-env '(:x :z) '(1 2) #H(:y 3))
      #H(:z 2 :x 1 :y 3) :test #'equalp)
  (is (extend-env '(:x :y) '(2 4) #H(:z 4))
      #H(:x 2 :y 4 :z 4) :test #'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apply

(subtest "si/apply"
  (is (si/apply '(:prim +) '(1 2 3))
      6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse

(subtest "closure-to-parameters-body-env"
  (is (closure-to-parameters-body-env '(:closure (:x :y) (:+ :x :y :z) #H(:z 4)))
      '((:x :y) (:+ :x :y :z) #H(:z 4))))

(subtest "let-to-parameters-args-body"
  (is (let-to-parameters-args-body '(:let ((:x 1) (:y 2)) (:+ :x :y)))
      '((:x :y) (1 2) (:+ :x :y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval

(subtest "eval-lambda" 
  (is (eval-lambda '(:lambda (:x :y) (:+ :x :y)) *env*)
      `(:closure (:x :y) (:+ :x :y) ,*env*)))

(subtest "eval-list"
  (is (eval-list '(1 2 3 4) *env*)
      '(1 2 3 4)))

(subtest "_eval"
  (is (_eval '(:+ :x :y) #H(:x 2 :y 4 :z 4 :+ '(:prim +)))
      6))

(subtest "si/eval"
  (is (si/eval "3")
      3)
  (is (si/eval "3.2")
      3.2)
  (is (si/eval "100/20")
      5)
  (is (si/eval "(:+ 1 2)")
      3)
  (is (si/eval "((:lambda (:a :b) (:+ :a :b)) 3 2)")
      5)
  (is (si/eval "((:lambda (:x :y :z) (:+ :x :y :z)) 3 2 5)")
      10)
  (is (si/eval "(:let ((:z 3))
                  ((:lambda (:x :y) (:+ :x :y :z))
                   2 5))")
      10)
  (is (si/eval "(:let ((:x 3))
                  (:let ((:fun (:lambda (:y) (:+ :x :y))))
                    (:+ (:fun 1) (:fun 2))))")
      9))

(finalize)
