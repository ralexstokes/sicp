#| Exercise 2.17.
   Define a procedure last-pair that returns the list that contains only the last element of a given (nonempty) list:
|#

(define nil '())

(define last-pair (items)
  (if (null? (cdr items))
      (list (car items))
      (last-pair (cdr items)))

#| example
(last-pair (list 23 72 149 34))
(34)
#|

