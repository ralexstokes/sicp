;; Exercise 3.70.  It would be nice to be able to generate streams in which the pairs appear in some useful order, rather than in the order that results from an ad hoc interleaving process. We can use a technique similar to the merge procedure of exercise 3.56, if we define a way to say that one pair of integers is ``less than'' another. One way to do this is to define a ``weighting function'' W(i,j) and stipulate that (i1,j1) is less than (i2,j2) if W(i1,j1) < W(i2,j2). Write a procedure merge-weighted that is like merge, except that merge-weighted takes an additional argument weight, which is a procedure that computes the weight of a pair, and is used to determine the order in which elements should appear in the resulting merged stream. Using this, generalize pairs to a procedure weighted-pairs that takes two streams, together with a procedure that computes a weighting function, and generates the stream of pairs, ordered according to weight.

;; prelude

(define (take s n)
  (if (= n 0) '()
      (cons (stream-car s)
            (take (stream-cdr s) (- n 1)))))

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

;;

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car
                               (merge-weighted (stream-cdr s1)
                                               (stream-cdr s2)
                                               weight))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

;; Use your procedure to generate:

;; a. the stream of all pairs of positive integers (i,j) with i < j ordered according to the sum i + j

(define (sum-pair p)
  (let ((i (car p))
        (j (cadr p)))
    (+ i j)))

(define sum-pairs
  (weighted-pairs integers integers sum-pair))

;; b. the stream of all pairs of positive integers (i,j) with i < j, where neither i nor j is divisible by 2, 3, or 5, and the pairs are ordered according to the sum 2 i + 3 j + 5 i j.

(define (b-sum p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))
(define (bnotdivis x)
  (cond ((= 0 (modulo x 2)) #f)
        ((= 0 (modulo x 3)) #f)
        ((= 0 (modulo x 5)) #f)
        (else #t)))
(define (bpredicate p)
  (let ((i (car p))
        (j (cadr p)))
    (and (bnotdivis i)
         (bnotdivis j))))
(define bstream
  (stream-filter bpredicate
                 (weighted-pairs integers integers b-sum)))
