#lang planet neil/sicp

(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a)
         (sum f (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube n)
  (* n n n))

;; 1.29
(define (simpsons f a b n)
  (let* ((n (if (even? n) n (+ n 1)))
         (h (/ (- b a) n))
         (h/3 (/ h 3.0)))
    (define (iter k s)
      (let* ((mul (cond
                   ((or (= k 0) (= k n)) 1)
                   ((even? k) 2)
                   (else 4)))
             (yk (+ a (* k h)))
             (s (+ s (* mul (f yk)))))
        (if (> k n)
            (* h/3 s)
            (iter (+ 1 k) s))))
    (iter 0 0)))

;; 1.30
(define (sum-1.30 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter 0 0))

;; 1.31
;; 1, recursive
(define (product1 f a next b)
  (if (> a b)
      1
      (* (f a)
         (product f (next a) next b))))

(define (factorial-recur n)
  (product1 (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))

;; 2, iterative
(define (product2 f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (f a) result))))
  (iter a 1))

(define (factorial-iter n)
  (product2 (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))

;; https://en.wikipedia.org/wiki/Wallis_product
;; to get close, n should be like 10,000
(define (find-pi n)
  (* 2 (product2 (lambda (x)
                   (* (/ (* 2.0 x) (- (* 2.0 x) 1))
                      (/ (* 2.0 x) (+ (* 2.0 x) 1))))
                 1
                 (lambda (x) (+ x 1))
                 n)))

;; 1.32
;; 1, iterative
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (factorial-accum-iter n)
  (accumulate-iter * 1 (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))

;; 2, recursive
(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recur combiner null-value term (next a) next b))))

(define (factorial-accum-recur n)
  (accumulate-recur * 1 (lambda (x) x) 1 (lambda (x) (+ 1 x)) n))
