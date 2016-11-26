;; Exercise 3.69.  Write a procedure triples that takes three infinite streams, S, T, and U, and produces the stream of triples (Si,Tj,Uk) such that i <= j <= k. Use triples to generate the stream of all Pythagorean triples of positive integers, i.e., the triples (i,j,k) such that i <= j and i2 + j2 = k2.

;; prelude

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (take s n)
  (if (= n 0) '()
      (cons (stream-car s)
            (take (stream-cdr s) (- n 1)))))

;;

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                        (stream-cdr (pairs t u)))
    (triples (stream-cdr s)
             (stream-cdr t)
             (stream-cdr u)))))

(define inc-int-triples
  (triples integers integers integers))

(define (pyth-triple? t)
  (let ((i (car t))
        (j (cadr t))
        (k (caddr t)))
    (= (square k)
       (+ (square i)
          (square j)))))

(define pyth (stream-filter pyth-triple? inc-int-triples))
