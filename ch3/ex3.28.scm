;; Exercise 3.28.  Define an or-gate as a primitive function box. Your or-gate constructor should be similar to and-gate.

(define (or-gate in1 in2 out)
  (define (logical-or s1 s2)
    (cond ((= s1 1) 1)
          ((= s2 1) 2)
          (else 0))
  (define (or-action)
    (let ((new-value
           (logical-or (get-signal in1) (get-signal in2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! out new-value)))))
  (add-action! in1 or-action)
  (add-action! in2 or-action)
  'ok)
