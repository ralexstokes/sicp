#| Exercise 3.7.
Consider the bank account objects created by make-account, with the password modification described in exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define a procedure make-joint that accomplishes this. Make-joint should take three arguments. The first is a password-protected account. The second argument must match the password with which the account was defined in order for the make-joint operation to proceed. The third argument is a new password. Make-joint is to create an additional access to the original account using the new password. For example, if peter-acc is a bank account with password open-sesame, then

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. You may wish to modify your solution to exercise 3.3 to accommodate this new feature.
|#

(define (contains set key)
  (cond ((null? set) #f)
        ((eq? key (car set)) #t)
        (else (contains (cdr set) key))))

(define (make-account balance secret)
  (let ((secrets (list secret)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password amount)
      "Incorrect password")
    (define (add-password password)
      (set! secrets (cons password secrets)))
    (define (valid-password? password)
      (contains secrets password))
    (define (dispatch password m)
      (cond ((not (valid-password? password)) incorrect-password)
            ((eq? m 'make-joint) add-password)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (make-joint acc password new-password)
  ((acc password 'make-joint) new-password)
  acc)
