;; Exercise 3.59.  We will represent the series a0 + a1 x + a2 x2 + a3 x3 + ··· as the stream whose elements are the coefficients a0, a1, a2, a3, ....

;; a. The integral of the series a0 + a1 x + a2 x2 + a3 x3 + ··· is the series ... where c is any constant. Define a procedure integrate-series that takes as input a stream a0, a1, a2, ... representing a power series and returns the stream a0, (1/2)a1, (1/3)a2, ... of coefficients of the non-constant terms of the integral of the series. (Since the result has no constant term, it doesn't represent a power series; when we use integrate-series, we will cons on the appropriate constant.)

;; prelude

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (take s n)
  (if (= n 0) '()
      (cons (stream-car s)
            (take (stream-cdr s) (- n 1)))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;;

(define (integrate-series s)
  (mul-streams recip-integers s))

(define recip-integers
  (stream-map (lambda (x) (/ 1 x)) integers))

;; b. The function x -> e^x is its own derivative. This implies that e^x and the integral of e^x are the same series, except for the constant term, which is e^0 = 1. Accordingly, we can generate the series for e^x as

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; Show how to generate the series for sine and cosine, starting from the facts that the derivative of sine is cosine and the derivative of cosine is the negative of sine:

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
