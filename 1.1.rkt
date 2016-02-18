#lang planet neil/sicp

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (fact x)
  (if (< x 1)
      1
      (* x (fact (- x 1)))))

(define (fact2 x)
  (define (iter y n)
    (if (< y 1)
        n
        (iter (- y 1) (* y n))))
  (iter x 1))

;; demonstration of lexical closure in action
(define (timesn n)
  (define (times x) (* x n))
  times)

(define times5 (timesn 5))

;; 1.3
(define (ex1.3 x y z)
  (apply sum-of-squares (cdr (sort (list x y z) <))))

;; 1.4
;; adds a to the absolute value of b
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 1.5: in an applicative-order interpreter, (test 0 (p)) will go into
;; an infinite loop. A normal-order interpreter would return 0.

;; 1.7
(define (sqrt-iter guess prevg x)
  (if (good-enough? guess prevg x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; 1.7
(define (good-enough? guess prevg x)
  (< (abs (/ (- guess prevg) guess)) 0.000001))


(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

;; 1.6 it will go into an infinite loop, since both clauses will
;; be unconditionally evaluated

;; 1.8
(define (cbrt x)
  (define (cbrt-iter guess prevg)
    (if (good-enough? guess prevg x)
        guess
        (cbrt-iter (improve-cbrt guess) guess)))
  (define (improve-cbrt guess)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (cbrt-iter 1.0 0.0))
