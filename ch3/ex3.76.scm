;; Exercise 3.76.  Eva Lu Ator has a criticism of Louis's approach in exercise 3.75. The program he wrote is not modular, because it intermixes the operation of smoothing with the zero-crossing extraction. For example, the extractor should not have to be changed if Alyssa finds a better way to condition her input signal. Help Louis by writing a procedure smooth that takes a stream as input and produces a stream in which each element is the average of two successive input stream elements. Then use smooth as a component to implement the zero-crossing detector in a more modular style.

;; prelude

(define (take s n)
  (cond ((empty-stream? s) '())
        ((= n 0) '())
        (else
         (cons (stream-car s)
               (take (stream-cdr s) (- n 1))))))

(define (stream-from-list l)
  (if (null? l)
      the-empty-stream
      (cons-stream
       (car l)
       (stream-from-list (cdr l)))))

(define (sign-change-detector next last)
  (cond ((negative? last)
         (if (positive? next) 1 0))
        ((or (positive? last)
             (zero? last))
         (if (negative? next) -1 0))
        (else 0)))

(define (average x y) (/ (+ x y) 2))

;;

(define (smooth s)
  (cond ((empty-stream? s) the-empty-stream)
        ((empty-stream? (stream-cdr s)) the-empty-stream)
        (else
         (let ((a (stream-car s))
               (b (stream-car (stream-cdr s))))
           (cons-stream (average a b)
                        (smooth (stream-cdr s)))))))

(define input '(1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4))
(define sense-data (stream-from-list input))
(define smoothed-sense-data (smooth sense-data))

(define zero-crossings
  (stream-map sign-change-detector smoothed-sense-data
              (cons-stream 0 smoothed-sense-data)))
