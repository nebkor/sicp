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
