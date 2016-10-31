#|  Exercise 2.33.
Fill in the missing expressions to complete the following definitions of some basic list-manipulation operations as accumulations:
|#

(define nil '())

(define (accumulate op val seq)
  (if (null? seq)
      val
      (op (car seq) (accumulate op val (cdr seq)))))

(define (-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
