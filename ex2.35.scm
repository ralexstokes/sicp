#| Exercise 2.35.
Redefine count-leaves from section 2.2.2 as an accumulation:
(define (count-leaves t)
  (accumulate <??> <??> (map <??> <??>)))
|#

(define nil '())

(define (count-leaves-old x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves-old (car x))
                 (count-leaves-old (cdr x))))))

(define (accumulate op val seq)
  (if (null? seq)
      val
      (op (car seq) (accumulate op val (cdr seq)))))

(define (flatten tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (flatten (car tree))
                      (flatten (cdr tree))))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (flatten t))))
