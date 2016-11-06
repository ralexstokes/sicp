;; Exercise 3.29.  Another way to construct an or-gate is as a compound digital
;; logic device, built from and-gates and inverters. Define a procedure or-gate
;; that accomplishes this. What is the delay time of the or-gate in terms of and-gate-delay and inverter-delay?

(define (or-gate in1 in2 out)
  (let ((a (make-wire))
        (b (make-wire))
        (c (make-wire)))
    (inverter in1 a)
    (inverter in2 b)
    (and-gate a b c)
    (inverter c out)
    'ok))

;; delay = 2 * inverter-delay + and-gate-delay
