#| Exercise 2.61.
Give an implementation of adjoin-set using the ordered representation. By analogy with element-of-set? show how to take advantage of the ordering to produce a procedure that requires on the average about half as many steps as with the unordered representation.
|#

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; can use ordering invariant to know when it is safe to stop scanning the list
;; `<` clause vs. `else` clause
;; on average, will save about half as many operations
