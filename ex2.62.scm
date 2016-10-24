;; Exercise 2.62.  Give a linear implementation of union-set for sets represented as ordered lists.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
            (cons (car set1)
                  (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2)
               (union-set (cdr set2) set1)))))
