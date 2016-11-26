;; Exercise 3.55.  Define a procedure partial-sums that takes as argument a stream S
;; and returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2, ....
;; For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, ....

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

;; utility

(define integers (cons-stream 1 (add-streams ones integers)))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))

(define (take s n)
  (if (= n 0) '()
      (cons (stream-car s)
            (take (stream-cdr s) (- n 1)))))
