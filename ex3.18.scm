#| Exercise 3.18/19
Write a procedure that examines a list and determines whether it contains a cycle, that is, whether a program that tried to find the end of the list by taking successive cdrs would go into an infinite loop. Exercise 3.13 constructed such lists.

Perform the check in constant space.
|#

;; Use Floyd's cycle finding algorithm to detect cycles
(define (has-cycle x)
  (define (iter tortoise hare)
    (cond ((null? hare) #f) ;; end of list
          ((eq? tortoise hare) #t) ;; cycle!
          ((null? (cdr tortoise)) #f) ;; check there is an element past the current tail
          (else
           (iter (cdr tortoise) (cddr hare)))))
  (cond ((null? x) #f)
        ((null? (cdr x)) #f)
        (else
         (iter (cdr x) (cddr x)))))

;; auxillary
(define (make-cycle x)
  (cond ((null? x) '())
        (else
         (set-cdr! (last-pair x) x)
         x)))

(define (range a b)
  (define (iter i)
    (if (> i b)
        '()
        (cons i
              (iter (+ 1 i)))))
  (iter a))

(define (list-of-length n)
  (if (= n 0)
      '()
      (cons n (list-of-length (- n 1)))))

(define lists (map list-of-length (range 1 10)))
(define cycles (map make-cycle lists))
(define has-cycles (map has-cycle cycles))

(define pass-tests
  (and
   (every (lambda (x) (if x #t #f)) has-cycles)
   (eq? #f (has-cycle '()))))
pass-tests
