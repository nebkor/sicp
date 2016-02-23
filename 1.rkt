#lang planet neil/sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION 1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (square n)
  (* n n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION 1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(define (h n) (A 2 n)) ;; 2^^n
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

;; 1.18
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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION 1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

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
    (define (y k)
      (f (+ a (* k h))))
    (define (term k)
      (* (y k)
         (cond
          ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4))))
    (define (next x) (+ 1 x))
    (* h/3 (sum term 0 next n))))

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
  (product1 (λ (x) x) 1 (λ (x) (+ 1 x)) n))

;; 2, iterative
(define (product2 f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (f a) result))))
  (iter a 1))

(define (factorial-iter n)
  (product2 (λ (x) x) 1 (λ (x) (+ 1 x)) n))

;; https://en.wikipedia.org/wiki/Wallis_product
;; to get close, n should be like 10,000
(define (find-pi n)
  (* 2 (product2 (λ (x)
                   (* (/ (* 2.0 x) (- (* 2.0 x) 1))
                      (/ (* 2.0 x) (+ (* 2.0 x) 1))))
                 1
                 (λ (x) (+ x 1))
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
  (accumulate-iter * 1 (λ (x) x) 1 (λ (x) (+ 1 x)) n))

;; 2, recursive
(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recur combiner null-value term (next a) next b))))

(define (factorial-accum-recur n)
  (accumulate-recur * 1 (λ (x) x) 1 (λ (x) (+ 1 x)) n))

;; 1.33
(define (filtered-accumulate combiner null-value term a next b pred?)
  (define (iter a result)
    (cond
     ((> a b) result)
     ((pred? a) (iter (next a) (combiner (term a) result)))
     (else (iter (next a) result))))
  (iter a null-value))

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 square a (λ (x) (+ 1 x)) b prime?))

;; home-rolled dumb version O(n)
(define (find-gcd a b)
  (define (iter d g)
    (if (or (> d a) (> d b))
        g
        (if (and (divides? d a) (divides? d b))
            (iter (+ 1 d) d)
            (iter (+ 1 d) g))))
  (iter 1 1))

;; or use gcd from section 1.2, O(log n)
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relatively-prime? a b)
  (= 1 (gcd a b)))

(define (ex1.33.2 n)
  (filtered-accumulate *                                 ; combiner
                       1                                 ; null value
                       (λ (x) x)                         ; term (identity)
                       1                                 ; a
                       (λ (x) (+ 1 x))                   ; next
                       n                                 ; b
                       (λ (x) (relatively-prime? x n)))) ; pred?

;; 1.34
(define (f g) (g 2))
;; (f f) -> (f 2) -> (2 2)

;; 1.35
;; use the given fixed-point function to find phi,
;; where phi is the fixed point value of f(x) = 1 + 1/x

(define fp-tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       fp-tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (ad-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       fp-tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (let ((next (/ (+ next guess) 2)))
        (if (close-enough? guess next)
            next
            (try next)))))
  (try first-guess))

;; racket@> (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1)
;; 1.6180327868852458

;; 1.36
;; racket@> (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;; 2.0
;; 9.965784284662087
;; 3.004472209841214
;; 6.279195757507157
;; 3.759850702401539
;; 5.215843784925895
;; 4.182207192401397
;; 4.8277650983445906
;; 4.387593384662677
;; 4.671250085763899
;; 4.481403616895052
;; 4.6053657460929
;; 4.5230849678718865
;; 4.577114682047341
;; 4.541382480151454
;; 4.564903245230833
;; 4.549372679303342
;; 4.559606491913287
;; 4.552853875788271
;; 4.557305529748263
;; 4.554369064436181
;; 4.556305311532999
;; 4.555028263573554
;; 4.555870396702851
;; 4.555315001192079
;; 4.5556812635433275
;; 4.555439715736846
;; 4.555599009998291
;; 4.555493957531389
;; 4.555563237292884
;; 4.555517548417651
;; 4.555547679306398
;; 4.555527808516254
;; 4.555540912917957
;; 4.555532270803653

;; with average-damping
;; racket@> (ad-fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;; 2.0
;; 5.9828921423310435
;; 4.922168721308343
;; 4.628224318195455
;; 4.568346513136242
;; 4.5577305909237005
;; 4.555909809045131
;; 4.555599411610624
;; 4.5555465521473675
;; 4.555537551999825
;; 4.555534487262465

;; without average dampening, it's 35 iterations; with, it's 11.

;; 1.37
;; a C solution from Rosetta Code:
;; double a, b, r;
;; 	a = b = r = 0.0;

;; 	unsigned i;
;; 	for (i = expansions; i > 0; i--) {
;; 		a = f_a(i);
;; 		b = f_b(i);
;; 		r = b / (a + r);
;; 	}
;; 	a = f_a(0);

;; 	return a + r;


(define (cont-frac ni di k)
  (define (iter r i)
    (let* ((n (ni (+ 1 i)))
           (d (di i))
           (r (/ n (+ d r))))
      (if (= i 1)
          (/ n (+ r (di 0)))
          (iter r (- i 1)))))
  (iter 0.0 k))
