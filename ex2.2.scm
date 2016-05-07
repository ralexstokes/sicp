#| Exercise 2.2.

Consider the problem of representing line segments in a plane. Each segment is represented as a pair of points: a starting point and an ending point.

Define a constructor make-segment and selectors start-segment and end-segment that define the representation of segments in terms of points.

Furthermore, a point can be represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly, specify a constructor make-point and selectors x-point and y-point that define this representation.

Finally, using your selectors and constructors, define a procedure midpoint-segment that takes a line segment as argument and returns its midpoint (the point whose coordinates are the average of the coordinates of the endpoints).

|#

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p q)
  (cons p q))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (average x y) (/ (+ x y) 2))

(define (midpoint-segment s)
  (let* ((p (start-segment s))
         (q (end-segment s))
         (px (x-point p))
         (py (y-point p))
         (qx (x-point q))
         (qy (y-point q)))
    (make-point
     (average px qx)
     (average py qy))))

(define origin (make-point 0 0))
(define unit (make-point 1 1))

(define unit-segment (make-segment origin unit))

(define midpoint (midpoint-segment unit-segment))

(print-point midpoint)
;; (1/2,1/2)
