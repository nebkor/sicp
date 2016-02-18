#lang planet neil/sicp

(define (square n)
  (* n n))

;; SECTION 1.2

;; ex1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 1 10) => 1024
;; (A 2 4) => 65536
;; (A 3 3) => 65536

(define (f n) (A 0 n)) ;; 2n
(define (g n) (A 1 n)) ;; 2^n
(define (h n) (A 2 n)) ;; 2^(h(n - 1)) for n > 0
(define (k n) (* 5 n n)) ;; 5n^2

;; ex1.11
;; f ( n ) = n if n < 3
;; f ( n ) = f ( n − 1 ) + 2 f ( n − 2 ) + 3 f ( n − 3 ) if n ≥ 3

;; recursive
(define (ex1.11r n)
  (if (< n 3)
      n
      (+ (ex1.11r (- n 1))
         (* 2 (ex1.11r (- n 2)))
         (* 3 (ex1.11r (- n 3))))))

(define (ex1.11i n)
  (let loop ((a 2)
             (b 1)
             (c 0)
             (count 2))
    (cond
     ((< n 3) n)
     ((= n count) a)
     (else
      (loop (+ a (* 2 b) (* 3 c)) a b (+ 1 count))))))

(define (dr n)
  (define (iter a b c count)
    (if (< count 3)
        n
        (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (iter 2 1 0 n))

;; 1.12
(define (pascal r i)
  (cond
   ((or (< i 0) (> i r)) 0)
   ((= r 0) 1)
   (else
    (+ (pascal (- r 1) (- i 1))
       (pascal (- r 1) i)))))

;; 1.15
(define (cube x) (* x x x))
(define (p x)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (displayln angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; growth is log(angle)

;; 1.16

;; first, recursive:
(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

;; Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2), keep, along
;; with the exponent n and the base b , an additional state variable a ,and define
;; the state transformation in such a way that the product a*(b^n) is unchanged from
;; state to state.
(define (fast-expt-iter b n a)
  (cond
   ((= n 0) a)
   ((even? n) (fast-expt-iter (square b) (/ n 2) a))
   (else
    (fast-expt-iter b (- n 1) (* a b)))))
(define (expt b n)
  (fast-expt-iter b n 1))

;; 1.17 fast-expt but for multiplication
(define (double n)
  (* 2 n))
(define (halve n)
  (/ n 2))

(define (mul a b)
  (cond
   ((= b 0) 0)
   ((even? b) (double (mul a (halve b))))
   (else
    (+ a (mul a (- b 1))))))

(define (fast-mul a b)
  (define (iter x y acc)
    (cond
     ((or (= x 0) (= y 0)) acc)
     ((even? y) (iter (double x) (halve y) acc))
     (else
      (iter x (- y 1) (+ x acc)))))
  (iter a b 0))

;; 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0)
         b)
        ((even? count)
         (fib-iter a
                   b
                   ;; shamelessly stolen from http://www.billthelizard.com/2010/01/sicp-exercise-119-computing-fibonacci.html
                   (+ (square p) (square q))     ; compute p'
                   (+ (* 2 p q) (square q))   ; compute q'
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q)
                      (* a q)
                      (* a p))
                   (+ (* b p)
                      (* a q))
                   p
                   q
                   (- count 1)))))

;; 1.21
;; 199 and 1999 are prime; smallest divisor of 19999 is 7.
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next n)
    (if (= 2 n)
        3
        (+ 2 n)))
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (begin
        (display n)
        (report-prime (- (current-milliseconds)
                         start-time))
        #t)
      #f))

(define (report-prime elapsed-time)
  (begin
    (display " *** ")
    (display elapsed-time)
    (newline)))

(define (timed-primes-upto n)
  (let ((n1 (+ 1 n)))
    (let loop ((i 1))
      (when (< i n1)
        (start-prime-test i (current-milliseconds))
        (loop (+ 2 i)))))
  (newline))

(define (timed-n-primes-greater-than n gt)
  (let loop ((p 0)
             (st (current-milliseconds))
             (start-at (+ 1 gt)))
    (when (< p n)
        (if (start-prime-test start-at st)
            (loop (+ 1 p) (current-milliseconds) (+ 1 start-at))
            (loop p (current-milliseconds) (+ 1 start-at))))))

;; 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; 1.25
(define (expmod-new base exp m)
  (remainder (expt base exp) m))
;; not appropriate for large values, as in fermat-test, since it must
;; compute the remainder of n^n divided by m, and n^n is a huge number

;; 1.26
;; by using an explicit multiplication in the even case of expmod,
;; expmod will be recursively called twice, rather than just once in the case
;; of (square (expmod ...)), which is n^2 vs. n times.

;; 1.27
(define (a-tothe-n-congruent-mod-n? a n)
  (let ((rem (remainder a n))
        (exprem (expmod a n n)))
    (= rem exprem)))

(define (prime-or-carmichael? n)
  (let loop ((a 1))
    (let ((res (a-tothe-n-congruent-mod-n? a n)))
      (if (and (< a n) res)
          (loop (+ 1 a))
          res))))

(define (find-carmichaels-upto n)
  (let loop ((m 1))
    (when (<= m n)
      (when (and (not (prime? m)) (prime-or-carmichael? m))
        (println m))
      (loop (+ 1 m)))))

;; racket@> (find-carmichaels-upto 10000)
;; 561
;; 1105
;; 1729
;; 2465
;; 2821
;; 6601
;; 8911

;; 1.28
(define (check-square x m)
  (let ((rsx (remainder (square x) m)))
    (if (and (not (or (= x 1) (= x (- m 1))))
             (= rsx 1))
        0
        rsx)))

(define (expmod2 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-square (expmod2 base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod2 base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (let ((res (expmod2 a (- n 1) n)))
      (= res 1)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-correct-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fc-prime? n)
  (fast-correct-prime? n (ceiling (sqrt n))))
