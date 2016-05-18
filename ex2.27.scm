#| Exercise 2.27.

Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure that takes a list as argument and returns as its value the list with its elements reversed and with all sublists deep-reversed as well.

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

(define (deep-reverse coll)
  (reverse (map (lambda (x)
                  (if (list? x)
                      (reverse x)
                      x)) coll)))

;; (define x (list (list 1 2) (list 3 4)))
;; (deep-reverse x)
;; ((4 3) (2 1))
