;; Exercise 4.4.  Recall the definitions of the special forms and and or from chapter 1:

;; and: The expressions are evaluated from left to right. If any expression evaluates to false, false is returned; any remaining expressions are not evaluated. If all the expressions evaluate to true values, the value of the last expression is returned. If there are no expressions then true is returned.

;; or: The expressions are evaluated from left to right. If any expression evaluates to a true value, that value is returned; any remaining expressions are not evaluated. If all expressions evaluate to false, or if there are no expressions, then false is returned.

;; Install and and or as new special forms for the evaluator by defining appropriate syntax procedures and evaluation procedures eval-and and eval-or. Alternatively, show how to implement and and or as derived expressions.

(load "m-eval.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and (and-predicates exp)env))
        ((or? exp) (eval-or (or-predicates exp) env))
        ((and-deriv? exp) (eval (and->if exp) env))
        ((or-deriv? exp) (eval (or->if exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (and? exp) (tagged-list? exp 'and))
(define (and-deriv? exp) (tagged-list? exp 'and-deriv))
(define (and-predicates exp) (cdr exp))
(define (eval-and exps env)
  (cond ((null? exps) true)
        ((false? (eval (first-exp exps) env)) false)
        ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval-and (rest-exps exps) env))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-deriv? exp) (tagged-list? exp 'or-deriv))
(define (or-predicates exp) (cdr exp))
(define (eval-or exps env)
  (cond ((null? exps) false)
        ((true? (eval (first-exp exps) env)) (eval (first-exp exps) env))
        (else (eval-or (rest-exps exps) env))))

(define (and->if exp)
  (expand-and (and-predicates exp)))
(define (expand-and exps)
  (if (null? exps) 'true
      (make-if (first-exp exps)
               (expand-and (rest-exps exps))
               'false)))

(define (or->if exp)
  (expand-or (or-predicates exp)))
(define (expand-or exps)
  (if (null? exps) 'false
      (make-if (first-exp exps)
               (first-exp exps)
               (expand-or (rest-exps exps)))))
