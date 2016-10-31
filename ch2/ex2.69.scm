#| Exercise 2.69.
The following procedure takes as its argument a list of symbol-frequency pairs (where no symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of leaves. Successive-merge is the procedure you must write, using make-code-tree to successively merge the smallest-weight elements of the set until there is only one element left, which is the desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find yourself designing a complex procedure, then you are almost certainly doing something wrong. You can take significant advantage of the fact that we are using an ordered set representation.)
|#

;; from text
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (elements-of-set? xs set)
  (any
   (lambda (x) (boolean=? x #t))
   (map (lambda (x) (element-of-set? x set)) xs)))

;; problem solution
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (merge-leaves leaves)
  (display (car leaves))
  (display (cadr leaves))
  ;; makes a tree from the first two entries, then returns an ordered set
  (let ((tree (make-code-tree (car leaves)
                              (cadr leaves))))
    (if (null? (cddr leaves)) tree
           (reorder-leaves tree (cddr leaves)))))
(define (reorder-leaves tree leaves)
  (cond ((null? leaves) '())
        ((<= (weight tree) (weight (car leaves)))
         (cons tree leaves))
        (else (cons (car leaves)
                    (reorder-leaves tree (cdr leaves))))))

(define (single-leaf? leaves)
  (and (leaf? (car leaves))
       (null? (cdr leaves))))

;; assumes leaves has at least two pairs
(define (successive-merge leaves)
  (define (iter tree leaves)
    (if (single-leaf? leaves) (make-code-tree (car leaves)
                                              tree)
        (let ((new-tree (make-code-tree (car leaves)
                                        (cadr leaves))))
          (iter new-tree (reorder-leaves new-tree (cddr leaves))))))
  (iter '() leaves))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
