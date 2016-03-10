#lang planet neil/sicp

;; foundation functions
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (average x . l)
  (let ((len (length l))
        (s (apply + l)))
    (let ((sum (+ s x))
          (len (+ 1 len)))
      (exact->inexact (/ sum len)))))

(define nil '())

(define (atom? x)
  (not (pair? x)))

;; 2.1 supporting
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

;; 2.1 money shot
(define (make-rat n d)
  (let ((g (abs (gcd n d)))
        (neg (negative? (* n d))))
    (let ((n (if neg (- (abs n)) (abs n)))
          (d (abs d)))
      (cons (/ n g)
            (/ d g)))))

;; 2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment p)
  (car p))

(define (end-segment p)
  (cdr p))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (let ((ss (start-segment s))
        (es (end-segment s)))
    (let ((xs (x-point ss))
          (ys (y-point ss))
          (xe (x-point es))
          (ye (y-point es)))
      (let ((x (average xs xe))
            (y (average ys ye)))
        (make-point x y)))))

;; 2.3
(define (make-rectangle upper-left lower-right)
  (let ((ur (make-point (x-point lower-right) (y-point upper-left)))
        (ll (make-point (x-point upper-left) (y-point lower-right))))
    (list upper-left ur lower-right ll)))

(define (make-center-rect center w h)
  (let ((w/2 (/ w 2.0))
        (h/2 (/ h 2.0))
        (cx (x-point center))
        (cy (y-point center)))
    (let ((ul (make-point (- cx w/2) (+ cy h/2)))
          (ur (make-point (+ cx w/2) (+ cy h/2)))
          (lr (make-point (+ cx w/2) (- cy h/2)))
          (ll (make-point (- cx w/2) (- cy h/2))))
      (list ul ur lr ll))))

(define (rect-ur r)
  (cadr r))
(define (rect-lr r)
  (caddr r))
(define (rect-ul r)
  (car r))
(define (rect-ll r)
  (cadddr r))

(define (rect-width r)
  (abs (- (x-point (rect-ur r)) (x-point (rect-ul r)))))

(define (rect-height r)
  (abs (- (y-point (rect-ur r)) (y-point (rect-lr r)))))

(define (rect-area r)
  (* (rect-height r) (rect-width r)))

(define (rect-perim r)
  (+ (* 2 (rect-height r))
     (* 2 (rect-width r))))

;; racket@> (define cr (make-center-rect (make-point 2.5 1) 5 2))
;; racket@> (rect-area cr)
;; 10.0
;; racket@> (rect-perim cr)
;; 14.0
;; racket@> (define r (make-rectangle (make-point 0 2) (make-point 5 0)))
;; racket@> (rect-area r)
;; 10
;; racket@> (rect-perim r)
;; 14

;; 2.4
;; define kdr given kons and kar
(define (kons x y)
  (λ (m) (m x y)))

(define (kar z)
  (z (λ (p q) p)))

;; z is a procedure that takes a procedure that takes two arguments
;; and returns its second argument. z applies the original x and y
;; (from the kons call that created z) as arguments to the procedure
;; it gets from kdr.
(define (kdr z)
  (z (λ (p q) q)))

;; 2.5
;; show that we can represent pairs of non-negative integers
;; by expressing them as the product of (* (expt 2 a) (expt 3 b)),
;; where a and b are the integers.
(define (icons a b)
  (* (expt 2 a) (expt 3 b)))

(define (find-dividing-expt n base)
  (define (iter try)
    (if (= 0 (remainder n (expt base try)))
        (iter (+ 1 try))
        (- try 1)))
  (iter 1))

(define (icar c)
  (find-dividing-expt c 2))

(define (icdr c)
  (find-dividing-expt c 3))

;; 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (zero f)
  (λ (x) x))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; one -> (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; ((lambda (f) (lambda (x) (f ((n f) x)))) (lambda (f) (lambda (x) x)))
;; ((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x))) x)))))
;; (lambda (f) (lambda (x) (f x)))
;;(define one (lambda (f) (lambda (x) (f x))))

;; could also define like:
(define (one f)
  (λ (x) (f x)))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (inc n)
  (+ 1 n))

(define (add-church m n)
  (λ (f) (λ (x) ((m f) ((n f) x)))))

;; racket@> ((one inc) 0)
;; 1
;; racket@> ((two inc) 0)
;; 2
;; racket@> (((add-church one two) inc) 0)
;; 3

;; Section 2.1.4: Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;; 2.7
(define upper-bound cdr)
(define lower-bound car)

;; 2.8
(define (sub-interval a b)
  (make-interval (- (lower-bound a)
                    (upper-bound b))
                 (- (upper-bound a)
                    (lower-bound b))))

;; 2.9
(define (interval-width i)
  (/ (- (upper-bound i) (lower-bound i))
     2.0))

;; racket@> (define small (make-interval 0.5 1.5))
;; racket@> (define big (make-interval 25.0 25.2))
;; racket@> (interval-width (mul-interval small big))
;; 12.649999999999999
;; racket@> (interval-width (add-interval small big))
;; 0.5999999999999996

;; 2.10
(define (safe-div-interval x y)
  (let ((y (if (and (>= (upper-bound y) 0)
                    (<= (lower-bound y) 0))
               (make-interval 0 0)
               y)))
    (div-interval x y)))

;; racket@> (define large (make-interval 0.0 11.0))
;; racket@> (div-interval small large)
;; '(0.045454545454545456 . +inf.0)
;; racket@> (safe-div-interval small large)
;; /: division by zero

;; 2.11
;; fuck doing the case-analysis of mul-interval

;; 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (interval-center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2.0))

(define (make-center-percent c p)
  (let ((w (* c p 0.01)))
    (make-center-width c w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION 2.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

;; 2.18
;; append definition given in text:
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))

;; iterative solution naievly developed without knowledge of
;; previously given "append" definition; took only a minute or so
;; to write from scratch. I'm more comfortable thinking in terms
;; of explicitly accumulating a new thing to return, so this was
;; my first solution.
(define (revers l)
  (define (iter l r)
    (if (null? l)
        r
        (iter (cdr l) (cons (car l) r))))
  (iter l '()))

;; recursive solution found only after several hours of casually chewing
;; on the problem. absence of "append" procedure in my ready-to-hand
;; mental toolkit prevented me from coming up with the right solution
;; quickly.
;; also, having to construct a list object for the second argument to
;; "append" seemed like the wrong thing to do to me, aesthetically,
;; so much so that I had everything but that in place before I checked
;; http://www.billthelizard.com/2010/12/sicp-218-reversing-list.html. I
;; will try to have more confidence in what my intellect is telling me
;; with regard to things like types when writing code.
;;
(define (rr l)
  (if (null? l)
      l
      (append (rr (cdr l)) (list (car l)))))
;;
;; Also, see comments below about the "natural" behavior of functions
;; on sequences, with respect to recursive/iterative and cons'd/appended.

;; 2.19
(define (no-more? cl)
  (null? cl))

(define (first-denomination cl)
  (car cl))

(define (except-first-denomination cl)
  (cdr cl))

;; needed to define previous three to use in updated
;; given definition of "cc" below.
(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc
             amount
             (except-first-denomination
              coin-values))
            (cc
             (- amount
                (first-denomination
                 coin-values))
             coin-values)))))


;; 2.20
;; this one definitely feels like, "ok, here's some scheme syntax
;; we want you to learn; we don't need you to prove a theorem or
;; anything here, just learn how to do rest arguments in scheme."
(define (same-parity i . l)
  (let ((pred (if (even? i) even? odd?)))
    (cons i (filter pred l))))

;; 2.21
;; complete the following definitions
;;
(define (square-list-recursive items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-recursive (cdr items)))))

(define (square-list-map items)
  (map square items))

;; for funsies, do square-list-iter
;; this took some futzing. interestingly, in order to get the result list
;; *not* reversed, I had to use the same technique with "append" as
;; in the recursive reverse function, but there it was needed to reverse
;; the list.
(define (square-list-iter items)
  (define (iter items res)
    (if (null? items)
        res
        (iter (cdr items) (append res (list (square (car items)))))))
  (iter items '()))

;; so:
;; append + recursive procedure: reverses input order
;; append + iterative procedure: maintains input order
;; cons + recursive procedure: maintains inpute order
;; cons + iterative procedure: reverses input order

;; oh, and ps: I think it would be fewer operations in square-list-iter
;; to return (reverse res) and accumulate res in the iter call like:
;; (iter (cdr items) (cons (square (car items)) res))
;; per the above chart, but my initial thought was that it would be
;; wasteful to construct a result list twice. given the above definition
;; of "append", I think there are more conses done in the version using
;; "append".

;; 2.22
;; Basically, do what I did for funsies :)
;;
(define (square-list-improper items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square
                     (car things))))))
  (iter items nil))

;; The second version given as an example results in a construct like:
;; (cons '() (cons (square (car items)) (cons (square (cadr items ....
;; which is an improper list

;; 2.23
(define (foreach f l)
  (if (null? l)
      (void)
      ((λ () ;; idiomatic Scheme would be to use a (begin ...) statement
          (f (car l))
          (foreach f (cdr l))))))

;; digression: count-leaves
;; given definition, recursive:
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; 2.24
;; nah, skip drawing it out

;; 2.25
;; (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
;; (car (car '((7))))
;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;; 2.26
;; racket@> (append x y)
;; '(1 2 3 4 5 6)
;; racket@> (cons x y)
;; '((1 2 3) 4 5 6)
;; racket@> (list x y)
;; '((1 2 3) (4 5 6))

;; 2.27
(define (deep-reverse l)
  (if (atom? l)
      l
      (append (deep-reverse (cdr l)) (list (deep-reverse (car l))))))

;; racket@> (deep-reverse '((1 (2 3) 4) 5 6 (((7) 8) 9) 10))
;; '(10 (9 (8 (7))) 6 5 (4 (3 2) 1))

(define (mk-counter n)
  (λ ()
    (set! n (+ 1 n))
    (displayln n)))

;; 2.28
;; first pass
(define (fringe t)
  (define (iter t res c)
    (c)
    (cond
     ((null? t) res)
     ((atom? t) (cons t res))
     (else
      (append (iter (car t) res c)
              (iter (cdr t) '() c)))))
  (iter t '() (mk-counter 0)))

;; fully recursive
(define (fringe-recursive t c)
  (c)
  (cond
   ((null? t) t)
   ((atom? t) (list t))
   (else
    (append (fringe-recursive (car t) c)
            (fringe-recursive (cdr t) c)))))

;; most iterative?
(define (fringe-iter t)
  (define (iter t res c)
    (c)
    (cond
     ((null? t) res)
     ((atom? t) (cons t res))
     (else
      (iter (car t) (iter (cdr t) res c) c))))
  (iter t '() (mk-counter 0)))

;; All three solutions take 43 steps to flatten the list
;; '(((1) 2 (3 4)) 5 6 (7 ((8 ((9 10) 11)))) 12 13), As given by the output
;; of the counter during running. However, "append" might be more expensive
;; than "cons".

;; 2.29
;; given make-mobile and make-branch
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; define left-branch, right-branch, branch-length, branch-structure,
;; and total-weight
(define (left-branch m)
  (car m))

(define (right-branch m)
  (cdr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cdr b))

(define (total-weight m)
  (let ((l (left-branch m))
        (r (right-branch m)))
    (let ((ls (branch-structure l))
          (rs (branch-structure r)))
      (cond
       ((and (atom? ls) (atom? rs)) (+ ls rs))
       ((atom? ls) (+ ls (total-weight rs)))
       ((atom? rs) (+ rs (total-weight ls)))
       (else
        (+ (total-weight ls) (total-weight rs)))))))

(define test-mobile (make-mobile
                     (make-branch 3 8)
                     (make-branch 2
                                  (make-mobile (make-branch 1 6)
                                               (make-branch 1 6)))))

;; fuck it, I'm using racket-isms and shit like let*
(define (balanced-mobile? m)
  (let* [(l (left-branch m))
         (r (right-branch m))
         (ll (branch-length l))
         (rl (branch-length r))
         (ls (branch-structure l))
         (rs (branch-structure r))]
    (cond
     [(and (atom? ls) (atom? rs)) (= (* ll ls) (* rl rs))]
     [(atom? ls) (and (= (* ll ls)
                         (* rl (total-weight rs)))
                      (balanced-mobile? rs))]
     [(atom? rs) (and (= (* rl rs)
                         (* ll (total-weight ls)))
                      (balanced-mobile? ls))]
     [else
      (and (= (* ll (total-weight ls))
              (* rl (total-weight rs)))
           (balanced-mobile? ls)
           (balanced-mobile? rs))])))

;; try using some smaller functions composed together
(define (branch-weight b)
  (let [(s (branch-structure b))]
    (if (pair? s)
        (tw s)
        s)))

;; this total weight function is definitely better than the first one
(define (tw m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))



(define (branch-torque b)
  (* (branch-length b) (branch-weight b)))

(define (branch-balanced? b)
  (let [(s (branch-structure b))]
    (if (pair? s)
        (bm? s)
        #t)))

;; with the two helper functions, this solution is not really shorter
;; than the first balanced-mobile function, but the intent is much
;; more clear.
(define (bm? m)
  (let* [(l (left-branch m))
         (r (right-branch m))
         (lt (branch-torque l))
         (rt (branch-torque r))]
    (and (= lt rt)
         (branch-balanced? l)
         (branch-balanced? r))))

;; 2.30
;; first just use the scale-tree implementation as a start
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

;; now with map
(define (square-tree-map tree)
  (map (λ (x)
         (if (pair? x)
             (square-tree-map x)
             (square x)))
       tree))

;; 2.31
(define (tree-map f tree)
  (map (λ (x)
         (if (pair? x)
             (tree-map f x)
             (f x)))
       tree))

(define (sq-tree tree)
  (tree-map square tree))

;; 2.32
(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
        (append rest (map (λ (x)
                            (cons (car s) x)) rest)))))

;; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

;; iterative accumulate from exercise 1.32
(define (fold-left op initial sequence)
  (define (iter l res)
    (if (null? l)
        res
        (iter (cdr l) (op res (car l)))))
  (iter sequence initial))

(define (acc-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (acc-length sequence)
  (accumulate (λ (x y) (+ 1 y)) 0 sequence))

(define dl '(((1) 2 (3 4)) 5 6 (7 ((8 ((9 10) 11)))) 12 13))

;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ (* x higher-terms) this-coeff))
   0
   coefficient-sequence))

;; 2.35
(define (acc-count-leaves t)
  (accumulate + 0 (map (λ (x) 1) (fringe t))))
;; better would be "(length (fringe t))"
