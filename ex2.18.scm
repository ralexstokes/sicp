#| Exercise 2.18.

Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:

(reverse (list 1 4 9 16 25))
(25 16 9 4 1)

|#

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

(define (reverse items)
  (if (null? items)
      '()
      (append (reverse (cdr items))
              (list (car items)))))

;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)
