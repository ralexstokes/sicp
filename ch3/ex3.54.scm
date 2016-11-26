;; Exercise 3.54.  Define a procedure mul-streams, analogous to add-streams,
;; that produces the elementwise product of its two input streams.

;; Use this together with the stream of integers to complete the following
;; definition of the stream whose nth element (counting from 0) is n + 1 factorial:

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1
                                (mul-streams factorials (stream-cdr integers))))

;;

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (take s n)
  (if (= n 0) '()
      (cons (stream-car s)
            (take (stream-cdr s) (- n 1)))))
