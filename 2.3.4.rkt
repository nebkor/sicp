#lang racket

;; helper
(define (member? val lst)
  (not (equal? #f (member val lst))))

;;;;;;;;;;;;;;;;;;;
;; Section 2.3.4: Huffman encoding trees
;;;;;;;;;;;;;;;;;;;

;; given:
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

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

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit:
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let [(pair (car pairs))]
        (adjoin-set
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

;; Exercise 2.67: Define an encoding tree and a sample message:

;; (define sample-tree
;;   (make-code-tree
;;    (make-leaf 'A 4)
;;    (make-code-tree
;;     (make-leaf 'B 2)
;;     (make-code-tree
;;      (make-leaf 'D 1)
;;      (make-leaf 'C 1)))))

;; (define sample-message
;;   '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;; Use the decode procedure to decode the message, and give the result.

;; racket@2.3.4.rkt> (decode sample-message sample-tree)
;; '(A D A B B C A)

;; given:
(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message) tree)
       (encode (cdr message) tree))))

;; 2.68: implement encode-symbol, use the given sample-message and sample-tree
;; to test the finished encode function.

;; First pass with iterative/accumulator
(define (encode-symbol-iter sym tree)
  (define (iter t res)
    (if (leaf? t)
        (reverse res)
        (let ([left (left-branch t)]
              [right (right-branch t)])
          (if (member? sym (symbols left))
              (iter left (cons 0 res))
              (iter right (cons 1 res))))))
  (if (not (member? sym (symbols tree)))
      (error (~a "Symbol '" sym "' is not in the given tree."))
      (iter tree '())))

;; now fully recursive solution; shorter and cleaner.
(define (encode-symbol sym tree)
  (if (leaf? tree)
      '()
      (let ([left (left-branch tree)]
            [right (right-branch tree)])
        (cond
         [(member? sym (symbols left)) (cons 0 (encode-symbol sym left))]
         [(member? sym (symbols right)) (cons 1 (encode-symbol sym right))]
         [else (error (~a "Symbol '" sym "' is not in the given tree."))]))))

;; racket@2.3.4.rkt> (equal? sample-message
;;                           (encode (decode sample-message sample-tree) sample-tree))
;; #t

;; 2.69. Given the following generate-huffman-tree function, implement
;; the successive-merge function to complete it.
(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
       (adjoin-set
        (make-code-tree (car leaf-set) (cadr leaf-set))
        (cddr leaf-set)))))

;; 2.70: some cutesy shit with old songs
(define job-tree
  (generate-huffman-tree
   '((wah 1) (boom 1) (a 2) (job 2) (get 2) (sha 3) (yip 9) (na 16))))

(define job-song
  (map string->symbol
       (string-split
        "get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom")))

;; racket@2.3.4.rkt> job-tree
;; '((leaf na 16)
;;   ((leaf yip 9)
;;    (((leaf a 2) ((leaf boom 1) (leaf wah 1) (boom wah) 2) (a boom wah) 4)
;;     ((leaf sha 3) ((leaf get 2) (leaf job 2) (get job) 4) (sha get job) 7)
;;     (a boom wah sha get job)
;;     11)
;;    (yip a boom wah sha get job)
;;    20)
;;   (na yip a boom wah sha get job)
;;   36)
;; racket@2.3.4.rkt> (string-join (map number->string (encode job-song job-tree)) "")
;; "111101100111111110000000001111011001111111100000000011011101010101010101010111011010"
;;
;; That's 84 bits for the huffman encoding for that song and tree. Without
;; huffman encoding, that would be 84 * 3 bits for the shortest possible
;; fixed-length encoding.

;; 2.71: not sketching it out, but in general, most frequent symbol requires
;; only one bit, and least-frequent requires log(n) bits.
