#| Exercise 3.23.  A deque (``double-ended queue'') is a sequence in which items can be inserted and deleted at either the front or the rear. Operations on deques are the constructor make-deque, the predicate empty-deque?, selectors front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!. Show how to represent deques using pairs, and give implementations of the operations. All operations should be accomplished in O(1) steps.
|#

;; in order to implement a deque, let's use a doubly linked list

;; a node in the doubly-linked list has a value, a forward pointer and a backward pointer
(define (make-node val fwd bwd)
  (list val fwd bwd))
(define (node-val n)
  (car n))
(define (node-fwd-ptr n)
  (cadr n))
(define (node-set-fwd-ptr! n item)
  (set-car! (cdr n) item))
(define (node-bwd-ptr n)
  (caddr n))
(define (node-set-bwd-ptr! n item)
  (set-car! (cddr n) item))

;; a deque consists of a pair with front and rear pointers to a doubly-linked list

(define (make-deque)
  (cons '() '()))

;; consider a deque empty if the front pointer is nil
(define (empty-deque? d)
  (and (null? (front-pointer-deque d))))

(define (front-pointer-deque d)
  (car d))
(define (rear-pointer-deque d)
  (cdr d))

(define (front-deque d)
  (if (empty-deque? d)
      (error "front-deque called with an empty deque" d)
      (node-val (front-pointer-deque d))))
(define (rear-deque d)
  (if (empty-deque? d)
      (error "rear-deque called with an empty deque" d)
      (node-val (rear-pointer-deque d))))

(define (front-insert-deque! d item)
  (let ((new (make-node item '() '())))
    (cond ((empty-deque? d)
             (set-car! d new)
             (set-cdr! d new)
             d)
          (else
           (let ((first-node (front-pointer-deque d)))
             (set-car! d new)
             (node-set-bwd-ptr! first-node new)
             (node-set-fwd-ptr! new first-node)
             d)))))

(define (rear-insert-deque! d item)
  (let ((new (make-node item '() '())))
    (cond ((empty-deque? d)
           (set-car! d new)
           (set-cdr! d new)
           d)
          (else
           (let ((last-node (rear-pointer-deque d)))
             (set-cdr! d new)
             (node-set-bwd-ptr! new last-node)
             (node-set-fwd-ptr! last-node new)
             d)))))

(define (front-delete-deque! d)
  (cond ((empty-deque? d)
         (error "Called front-delete-deque! with an empty deque" d))
        (else
         (let ((first-node (front-pointer-deque d)))
           (set-car! d (node-fwd-ptr first-node))
           (node-set-bwd-ptr! (front-pointer-deque d) '())
           d))))

(define (rear-delete-deque! d)
  (cond ((empty-deque? d)
         (error "Called rear-delete-deque! with an empty deque" d))
        (else
         (let ((last-node (rear-pointer-deque d)))
           (set-cdr! d (node-bwd-ptr last-node))
           (node-set-fwd-ptr! (rear-pointer-deque d) '())
           d))))

;; tests
(define (empty-deque-is-empty)
  (let ((d (make-deque)))
    (eq? #t (empty-deque? d))))
(define (can-insert-front)
  (let ((d (make-deque)))
    (front-insert-deque! d 'a)
    (eq? 'a (front-deque d))
    (front-insert-deque! d 'b)
    (eq? 'b (front-deque d))))
(define (can-insert-rear)
  (let ((d (make-deque)))
    (rear-insert-deque! d 'a)
    (eq? 'a (rear-deque d))
    (rear-insert-deque! d 'b)
    (eq? 'b (rear-deque d))))
(define (can-delete-front)
  (let ((d (make-deque)))
    (front-insert-deque! d 'a)
    (front-insert-deque! d 'b)
    (front-delete-deque! d)
    (eq? 'a (front-deque d))))
(define (can-delete-rear)
  (let ((d (make-deque)))
    (rear-insert-deque! d 'a)
    (rear-insert-deque! d 'b)
    (rear-delete-deque! d)
    (eq? 'a (rear-deque d))))
(define (tests)
  (and (empty-deque-is-empty)
       (can-insert-front)
       (can-insert-rear)
       (can-delete-front)
       (can-delete-rear)))
(if (tests)
    'pass
    'fail)
