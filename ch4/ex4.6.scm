;; Exercise 4.6.  Let expressions are derived expressions, because

;;(let ((<var1> <exp1>) ... (<varn> <expn>))
;;  <body>)
;;
;;is equivalent to
;;
;;((lambda (<var1> ... <varn>)
;;   <body>)
;; <exp1>
;;
;; <expn>)
;;
;;Implement a syntactic transformation let->combination that reduces evaluating let expressions to evaluating combinations of the type shown above, and add the appropriate clause to eval to handle let expressions.

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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (binding-var binding) (car binding))
(define (binding-exp binding) (cadr binding))
(define (unzip-bindings bindings)
  ;; NOTE doesn't handle mismatch in cardinality of vars and exps
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
