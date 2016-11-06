;; Exercise 3.24.  In the table implementations above, the keys are tested for equality using equal? (called by assoc).
;; This is not always the appropriate test. For instance, we might have a table with numeric keys in which we don't
;; need an exact match to the number we're looking up, but only a number within some tolerance of it. Design a table
;; constructor make-table that takes as an argument a same-key? procedure that will be used to test "equality" of keys.
;; Make-table should return a dispatch procedure that can be used to access appropriate lookup and insert! procedures for a local table.

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table))))
        'ok))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Operation unknown -- TABLE" m) )))
    dispatch))

(define (tests)
  (define (same-key? x y)
    (<
     (abs
      (- x y))
     0.1))
  (define (factory)
    (let ((table (make-table same-key?)))
      (let ((put (table 'insert-proc!))
            (get (table 'lookup-proc)))
        (cons put get))))
  (define (can-read-write)
    (let* ((put-get (factory))
           (put (car put-get))
           (get (cdr put-get)))
      (put 1.0 'a)
      (put 2.0 'b)
      (and (eq? (get 1.0) 'a)
           (eq? (get 2.0) 'b))))
  (define (keys-merge)
    (let* ((put-get (factory))
           (put (car put-get))
           (get (cdr put-get)))
      (put 1.0 'a)
      (put 1.05 'c)
      (put 2.0 'b)
      (put 2.1 'd)
      (and (eq? (get 1.0) 'c)
           (eq? (get 2.0) 'b))))
  (and (can-read-write)
       (keys-merge)))

(if (tests)
    'pass
    'fail)
