(in-package :cl21-user)
(defpackage scheme-impl
  (:use #:cl21
        #:cl-annot
        #:anaphora))
(in-package :scheme-impl)

(annot:enable-annot-syntax)

;; 参考: http://tatsu-zine.com/samples/free/scheme-in-ruby.pdf
;; これを CL で実装する。構文を [:+, x, y] にしている理由が Ruby で簡単に読み込めるためということで、今回は (:+ x y) の構文に置きかえた

;; 開発時用
;; M-x slime-restart-inferior-lisp
;; ↓
(defun in-env ()
  (ql:quickload :scheme-impl)
  (in-package :scheme-impl)
  (asdf:test-system :scheme-impl))

(defun run-test ()
  (asdf:test-system :scheme-impl))
;; 

(defparameter *primitive-fun-env*
  #H(:+ '(:prim +)
     :- '(:prim -)
     :* '(:prim *)))

@export
(defun lookup-primitive-fun (exp)
  (aif (getf *primitive-fun-env* exp)
       it
       (error "~S is undefined function or don't expected type" exp)))

(defun immediate-val-p (exp)
  (numberp exp))

@export
(defun apply-primitive-fun (fun args)
  (let ((fun-val (second fun)))
    (apply fun-val args)))

(defun si/apply (fun args)
  (apply-primitive-fun fun args))

@export
(defun si/eval (string-exp)
  (assert (stringp string-exp))
  (_eval (read-from-string string-exp)))

(defun _eval-list (exp)
  (map #'_eval exp))

(defun _eval (exp)
  (if (listp exp)
      (let ((fun (_eval (first exp)))
            (args (_eval-list (rest exp))))
        (si/apply fun args))
      (if (immediate-val-p exp)
          exp
          (lookup-primitive-fun exp))))
