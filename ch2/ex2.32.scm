#| Exercise 2.32.

We can represent a set as a list of distinct elements, and we can represent the set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure that generates the set of subsets of a set and give a clear explanation of why it works:

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map <??> rest)))))

|#

(define nil '())

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s)
                                  x))
                          rest)))))

;; intuition: recursively adding the first element of the current traversal through the list to each of the generated subsets (for each successive tail of the list)
;; see http://www.billthelizard.com/2011/03/sicp-232-generating-power-sets.html and https://en.wikipedia.org/wiki/Power_set#Algorithms for more info
