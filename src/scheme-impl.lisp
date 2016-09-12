(in-package :cl21-user)
(defpackage scheme-impl
  (:use #:cl21
        #:cl-annot
        #:anaphora
        #:cl-dbc-lclj)
  (:import-from #:alexandria
                #:plist-hash-table))
(in-package :scheme-impl)

(annot:enable-annot-syntax)

;; 参考: http://tatsu-zine.com/samples/free/scheme-in-ruby.pdf
;; これを CL で実装する。構文を [:+, x, y] にしている理由が Ruby で簡単に読み込めるためということで、今回は (:+ :x :y) の構文に置きかえた
;; 関数名、変数名を共にキーワードとする

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 開発用
;; M-x slime-restart-inferior-lisp
;; ↓

(defun in-env ()
  (ql:quickload :scheme-impl)
  (in-package :scheme-impl)
  (asdf:test-system :scheme-impl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables

(defparameter *primitive-fun-env*
  '(:+  (:prim +)
    :-  (:prim -)
    :*  (:prim *)
    :>  (:prim >)
    :>= (:prim >=)
    :<  (:prim <)
    :<= (:prim <=)
    :== (:prim =)))

(defparameter *boolean-env*
  '(:true t
    :false nil))

@export
(defparameter *global-env* (plist-hash-table (append *primitive-fun-env* *boolean-env*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup

@export
(defunc lookup-var (var env)
  (:pre ((keywordp var) (hash-table-p env)))
  (aif (getf env var)
       it
       (error "couldn't find value to variables:~S" var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predicate

(defun immediate-val-p (exp)
  (numberp exp))

@export
(defun letp (exp)
  (and
   (eq :let (first exp))
   (listp (second exp))
   (third exp)))

(defun letrecp (exp)
  (and
   (eq :letrec (first exp))
   (listp (second exp))
   (third exp)))

(defun lambdap (exp)
  (and
   (eq :lambda (first exp))
   (listp (second exp))
   (third exp)))

(defun ifp (exp)
  (and
   (eq :if (first exp))
   (second exp)
   (third exp)
   (fourth exp)))

@export
(defunc special-form-p (exp)
  (:pre ((consp exp)))
  (funcall #'(or letp lambdap ifp letrecp) exp))

(defun primitive-fun-p (exp)
  (eq (first exp) :prim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env

@export
(defunc extend-env (parameters args env)
  (:pre ((listp parameters) (listp args) (hash-table-p env)))
  (let ((new-env #H()))
    (loop for value being the hash-values of env using (hash-key key)
          :do (setf (getf new-env key) value))
    (loop :for x :in parameters
          :for y :in args
          :do (setf (getf new-env x) y))
    new-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; closure

(defunc make-closure (exp env)
  (:pre ((listp exp) (hash-table-p env)))
  (let ((parameters (second exp))
        (body (third exp)))
    (list :closure parameters body env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apply

@export
(defunc apply-primitive-fun (fun args)
  (:pre ((primitive-fun-p fun) (listp args)))
  (let ((fun-val (second fun)))
    (apply fun-val args)))

@export
(defunc apply-lambda (closure args)
  (:pre ((listp closure) (listp args)))
  (destructuring-bind (parameters body env) (closure-to-parameters-body-env closure)
    (let ((new-env (extend-env parameters args env)))
      (_eval body new-env))))

@export
(defun si/apply (fun args)
  (cond ((primitive-fun-p fun) (apply-primitive-fun fun args))
        (t (apply-lambda fun args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse

@export
(defun closure-to-parameters-body-env (closure)
  `(,(second closure) ,(third closure) ,(fourth closure)))

@export
(defun let-to-parameters-args-body (exp)
  (list (map #'first (second exp))
        (map #'second (second exp))
        (third exp)))

;; defun letrec-to-parameter-args-body
@export
(defun letrec-to-parameters-args-body (exp)
  (let-to-parameters-args-body exp))

@export
(defun if-to-cond-true-false (exp)
  `(,(second exp) ,(third exp) ,(fourth exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval

@export
(defunc si/eval (string-exp)
  (:pre ((stringp string-exp) (hash-table-p *global-env*)))
  (_eval (read-from-string string-exp) *global-env*))

@export
(defun eval-let (exp env)
  (destructuring-bind (parameters args body) (let-to-parameters-args-body exp)
    (let ((new-exp (append `((:lambda ,parameters ,body)) args)))
      (_eval new-exp env))))

(defun repeat (n obj)
  (if (= n 0)
      '()
      (cons obj (repeat (1- n) obj))))

@export
(defun extend-env-by-dummy (parameters env)
  (extend-env parameters (repeat (length parameters) :dummy) env))

;; サイズが大きくなる可能性のある環境の Hash-table の更新なので副作用を用いる？
(defun update-extend-env (parameters args-val ext-env)
  (map (lambda (param val) (setf (gethash ext-env param) val))
       parameters
       args-val))

@export
(defun eval-letrec (exp env)
  (destructuring-bind (parameters args body) (letrec-to-parameters-args-body exp)
;;    (declare (ignore body))
    (let* ((ext-env (extend-env-by-dummy parameters env))
           (args-val (eval-list args ext-env)))
      (update-extend-env parameters args-val ext-env)
      (_eval (list (list :lambda parameters body) (car args)) ext-env))))

@export
(defun eval-lambda (exp env)
  (make-closure exp env))

(defun eval-if (exp env)
  (destructuring-bind (condi true-clause false-clause) (if-to-cond-true-false exp)
    (if (_eval condi env)
        (_eval true-clause env)
        (_eval false-clause env))))

(defun eval-special-form (exp env)
  (cond ((lambdap exp) (eval-lambda exp env))
        ((letp exp) (eval-let exp env))
        ((letrecp exp) (eval-letrec exp env))
        ((ifp exp) (eval-if exp env))
        (t (error "~S is unmatched as special form." exp))))

@export
(defunc eval-list (exp env)
  (:pre ((listp exp) (hash-table-p env)))
  (map ^(_eval %1 env) exp))

@export
(defunc _eval (exp env)
  (:pre ((hash-table-p env) (or (consp exp) (keywordp exp) (numberp exp))))
  (if (consp exp)
      (if (special-form-p exp)
          (eval-special-form exp env)
          (let ((fun (_eval (first exp) env))
                (args (eval-list (rest exp) env)))
            (si/apply fun args)))
      (if (immediate-val-p exp)
          exp
          (lookup-var exp env))))
