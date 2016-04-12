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
        (else (error "unknown expression
                      type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

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
        (else (list '* m1 m2))))

;; 2.56: add exponent expressions

(define (exponentiation? exp)
  (and (pair? exp) (eq? '** (car exp))))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (make-exponentiation base exponent)
  (cond
   [(=number? base 0) 0]
   [(=number? exponent 0) 1]
   [else (list '** base exponent)]))