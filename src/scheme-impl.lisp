(in-package :cl21-user)
(defpackage scheme-impl
  (:use #:cl21
        #:cl-annot
        #:anaphora
        #:cl-dbc-lclj))
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
  #H(:+ '(:prim +)
     :- '(:prim -)
     :* '(:prim *)))

@export
(defparameter *global-env* *primitive-fun-env*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup

;; @export
;; (defun lookup-primitive-fun (exp)
;;   (aif (getf *primitive-fun-env* exp)
;;        it
;;        (error "~S is undefined function or don't expected type" exp)))

@export
(defun lookup-var (var env)
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

(defun lambdap (exp)
  (and
   (eq :lambda (first exp))
   (listp (second exp))
   (third exp)))

@export
(defun special-form-p (exp)
  (or (letp exp) (lambdap exp)))

(defun primitive-fun-p (exp)
  (eq (first exp) :prim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; env

@export
(defun extend-env (parameters args env)
  (let ((new-env #H()))
    (loop for value being the hash-values of env
            using (hash-key key)
          :do (setf (getf new-env key) value))
    (loop :for x :in parameters
          :for y :in args
          :do (setf (getf new-env x) y))
    new-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; closure

(defun make-closure (exp env)
  (let ((parameters (second exp))
        (body (third exp)))
    `(:closure ,parameters ,body ,env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apply

@export
(defun apply-primitive-fun (fun args)
  (let ((fun-val (second fun)))
    (apply fun-val args)))

@export
(defun apply-lambda (closure args)
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
  `(,(map #'first (second exp))
    ,(map #'second (second exp))
    ,(third exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval

@export
(defun si/eval (string-exp)
  (assert (stringp string-exp))
  (_eval (read-from-string string-exp) *global-env*))

@export
(defun eval-let (exp env)
  (destructuring-bind (parameters args body) (let-to-parameters-args-body exp)
    (let ((new-exp (append `((:lambda ,parameters ,body)) args)))
      (_eval new-exp env))))

@export
(defun eval-lambda (exp env)
  (make-closure exp env))

(defun eval-special-form (exp env)
  (cond ((lambdap exp) (eval-lambda exp env))
        ((letp exp) (eval-let exp env))
        (t (error "~S is unmatched as special form." exp))))

@export
(defun eval-list (exp env)
  (map ^(_eval %1 env) exp))

@export
(defun _eval (exp env)
  (if (listp exp)
      (if (special-form-p exp)
          (eval-special-form exp env)
          (let ((fun (_eval (first exp) env))
                (args (eval-list (rest exp) env)))
            (si/apply fun args)))
      (if (immediate-val-p exp)
          exp
          (lookup-var exp env))))
