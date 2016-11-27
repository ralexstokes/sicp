;; Exercise 4.7.  Let* is similar to let, except that the bindings of the let variables are performed sequentially from left to right, and each binding is made in an environment in which all of the preceding bindings are visible. For example

;; (let* ((x 3)
;;        (y (+ x 2))
;;        (z (+ x y 5)))
;;   (* x z))
;;
;; returns 39. Explain how a let* expression can be rewritten as a set of nested let expressions, and write a procedure let*->nested-lets that performs this transformation.

;; We can nest lets in the following way to achieve the semantics of let*:
;; A let* expression has a list of bindings and a body.
;; Define a let for the first binding in the list of bindings.
;; The body of this let will be a another let* that contains the remaining bindings.
;; Recursively expand the inner let* until there is only a final binding left.
;; This final binding produces a final let that contains the original body of the original let*.

;; For example:
;; (let* (b0 b1)
;;   <body>)
;; =>
;; (let ((b0-var b0-val))
;;   (let (b1-var b1-val)
;;     <body>))
;;

(load "m-eval.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*->nested-lets exp)
  (expand-let* (let-bindings exp) (let-body exp)))
(define (expand-let* bindings body)
  (if (null? bindings) body
      (make-let (list (car bindings))
                (expand-let* (cdr bindings) body))))

;; utilities from ex4.6
(define (let? exp) (tagged-list? exp 'let))
(define (make-let bindings body) (list 'let bindings body))
(define (let-bindings exp) (cadr exp))
(define (binding-var binding) (car binding))
(define (binding-exp binding) (cadr binding))
(define (unzip-bindings bindings)
  (define (iter bindings vars exps)
    (if (null? bindings) (cons vars exps)
        (iter (cdr bindings)
              (cons (binding-var (car bindings)) vars)
              (cons (binding-exp (car bindings)) exps))))
  (iter bindings '() '()))
(define (let-body exp) (caddr exp))

(define (let->combination exp)
  (expand-let (let-bindings exp) (let-body exp)))
(define (expand-let bindings body)
  (let ((vars-exps (unzip-bindings bindings)))
    (let ((vars (car vars-exps))
          (exps (cdr vars-exps)))
      (cons (make-lambda vars (list body))
            exps))))
