#lang racket

(define (atom? x)
  (not (pair? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION 2.3.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2.54: implement equal? in terms of eq?
(define (my-equal? a b)
  (cond
   [(and (atom? a) (atom? b)) (eq? a b)]
   [(and (pair? a) (pair? b))
    (let ([ca (car a)]
          [cb (car b)])
      (and (eq? ca cb)
           (my-equal? (cdr a) (cdr b))))]
   [else #f]))

;; 2.55: the expression ''foo expands into (quote (quote foo)),
;; or, '(quote foo); the car of that is 'quote

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION 2.3.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; given the following functions for derivative stuff:
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        [(exponentiation? exp)
         (let* ([b (base exp)]
                [e (exponent exp)]
                [d (deriv b var)])
           (make-product
            (make-product e
                          (make-exponentiation
                           b
                           (make-sum e -1)))
            d))]
        (else (error "unknown expression
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

;; 2.56: add exponent expressions

(define (exponentiation? exp)
  (and (pair? exp) (eq? '** (cadr exp))))

(define (base exp)
  (car exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exponent)
  (cond
   [(=number? exponent 0) 1]
   [(=number? exponent 1) base]
   [(=number? base 0) 0]
   [(=number? base 1) 1]
   [else (list base '** exponent)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION 2.3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; given in text:
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

;; 2.59: implement union-set
(define (union-set s1 s2)
  (cond
   [(null? s1) s2]
   [(null? s2) s1]
   [(element-of-set? (car s1) s2) (union-set (cdr s1) s2)]
   [else (union-set (cdr s1) (cons (car s1) s2))]))

;; 2.60: adjoin-set becomes unconditional (cons x set), and union-set becomes
;; (append s1 s2); the rest work as-is for non-unique-element sets

;; given in text for ordered set:
(define (element-of-ordered-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

;; 2.61: write order-preserving adjoin-set
(define (adjoin-ordered-set x s)
  (define (iter s acc done?)
    (cond
     [(and (not done?) (null? s)) (reverse (cons x acc))]
     [(null? s) (reverse acc)]
     [(= x (car s)) (iter (cdr s) (cons (car s) acc) #t)]
     [(and (not done?) (< x (car s)))
      (iter (cdr s) (cons (car s) (cons x acc)) #t)]
     [else (iter (cdr s) (cons (car s) acc) done?)]))
  (if (null? s)
      (list x)
      (iter s '() #f)))

;; 2.62: write O(n) union-ordered-set
(define (union-ordered-set s1 s2)
  (define (iter s1 s2 acc)
    (cond
     [(null? s2) (append (reverse acc) s1)]
     [(null? s1) (append (reverse acc) s2)]
     [else
      (let ([h1 (car s1)]
            [h2 (car s2)])
        (cond
         [(= h1 h2) (iter (cdr s1) (cdr s2) (cons h1 acc))]
         [(< h1 h2) (iter (cdr s1) s2 (cons h1 acc))]
         [else (iter s1 (cdr s2) (cons h2 acc))]))]))
  (iter s1 s2 '()))

(define (intersection-ordered-set s1 s2)
  (define (iter s1 s2 acc)
    (if (or (null? s2) (null? s1))
        (reverse acc)
        (let ([h1 (car s1)]
              [h2 (car s2)])
          (cond
           [(= h1 h2) (iter (cdr s1) (cdr s2) (cons h1 acc))]
           [(< h1 h2) (iter (cdr s1) s2 acc)]
           [else (iter s1 (cdr s2) acc)]))))
  (iter s1 s2 '()))

;; given in text for tree ops:
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-tree-set?
          x
          (left-branch set)))
        ((> x (entry set))
         (element-of-tree-set?
          x
          (right-branch set)))))

(define (adjoin-tree-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-tree-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-tree-set x (right-branch set))))))

;; 2.63: do the two given tree-flatteners produce the same results
;; for the same input?
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

;; the three trees from figure 2.16:
;;
;; > (define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
;; > (define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
;; > (define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
;; > (tree->list-1 t1)
;; '(1 3 5 7 9 11)
;; > (tree->list-1 t2)
;; '(1 3 5 7 9 11)
;; > (tree->list-1 t3)
;; '(1 3 5 7 9 11)
;; > (tree->list-2 t1)
;; '(1 3 5 7 9 11)
;; > (tree->list-2 t2)
;; '(1 3 5 7 9 11)
;; > (tree->list-2 t3)
;; '(1 3 5 7 9 11)
;;
;; I think both procedures are order-preserving, so they should return
;; the same output for a given input.

;; 2.64: given the following:
(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

;; let* for more readability
(define (pt tlist n)
  (if (= n 0)
      (cons '() tlist)
      (let* ([left-size (quotient (- n 1) 2)]
             [left-result (pt tlist left-size)]
             [left-tree (car left-result)]
             [non-left-elts (cdr left-result)]
             [right-size (- n (+ left-size 1))]
             [this-entry (car non-left-elts)]
             [right-result (pt (cdr non-left-elts) right-size)]
             [right-tree (car right-result)]
             [remaining-elts (cdr right-result)])
        (cons (make-tree this-entry
                         left-tree
                         right-tree)
              remaining-elts))))


;; 2.65: write O(n) union and intersection for tree-sets
(define (union-tree-set s1 s2)
  (let* ([l1 (tree->list-1 s1)]
         [l2 (tree->list-1 s2)]
         [ul (union-ordered-set l1 l2)])
    (pt ul (length ul))))

(define (intersection-tree-set s1 s2)
  (let* ([l1 (tree->list-1 s1)]
         [l2 (tree->list-1 s2)]
         [il (intersection-ordered-set l1 l2)])
    (pt il (length il))))

;; 2.66: implement lookup procedure for a database where the entries
;; are a set of records organized into a balanced tree with the entries'
;; keys serving as the ordering value

(define mkrecord cons)

(define (key record)
  (car record))

(define (body record)
  (cdr record))

(define (lookup given-key records)
  (cond
   [(null? records) false]
   [(= given-key (key (entry records))) (entry records)]
   [(< given-key (key (entry records))) (lookup
                                         given-key
                                         (left-branch records))]
   [(> given-key (key (entry records))) (lookup
                                         given-key
                                         (right-branch records))]))
