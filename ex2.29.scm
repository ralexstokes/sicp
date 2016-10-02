#| Exercise 2.29.

A binary mobile consists of two branches, a left branch and a right branch. Each branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can represent a binary mobile using compound data by constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

a.  Write the corresponding selectors left-branch and right-branch, which return the branches of a mobile, and branch-length and branch-structure, which return the components of a branch.

|#

(define nil '())

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (if (null? mobile) nil
      (car mobile)))

(define (right-branch mobile)
  (if (null? mobile) nil
      (car (cdr mobile))))

(define (branch-length branch)
  (if (null? branch) nil
      (car branch)))

(define (branch-structure branch)
  (if (null? branch) nil
      (car (cdr branch))))

#|
b.  Using your selectors, define a procedure total-weight that returns the total weight of a mobile.
|#

(define (terminal? branch)
  (number? (branch-structure branch)))

(define (total-weight-branch branch)
  (cond ((null? branch) 0)
        ((number? (branch-structure branch))
         (branch-structure branch))
        (else (total-weight branch))))

(define (total-weight mobile)
  (if (null? mobile) 0
      (+ (total-weight-branch (left-branch mobile))
         (total-weight-branch (right-branch mobile)))))

;; (define a (make-branch 1 1))
;; (define b (make-branch 1 2))
;; (define c (make-branch 22 2))

;; (define m (make-mobile a a))
;; (define n (make-mobile a c))
;; (define p (make-mobile m c))
;; (define q (make-mobile m m))

;; (total-weight m)
;; -> 2
;; (total-weight n)
;; -> 3
;; (total-weight p)
;; -> 4
;; (total-weight q)
;; -> 4

#|
c.  A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.
|#

(define (torque branch)
  (cond ((null? branch) 0)
        ((terminal? branch)
         (* (branch-structure branch)
            (branch-length branch)))
        (else 0)))

(define (mobile-balanced? mobile)
  (cond ((null? mobile) #f)
        ((terminal? mobile) #t)
        (else (and (= (torque (left-branch mobile))
                      (torque (right-branch mobile)))
                   (mobile-balanced? (left-branch mobile))
                   (mobile-balanced? (right-branch mobile))))))

;; (mobile-balanced? m)
;; -> #f
;; (mobile-balanced? n)
;; -> #f
;; (mobile-balanced? p)
;; -> #f
;; (mobile-balanced? q)
;; -> #t

#|
d.  Suppose we change the representation of mobiles so that the constructors are

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

How much do you need to change your programs to convert to the new representation?
|#

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))
