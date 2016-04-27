#lang racket

;; 3.1, write a accumulator closure generator
(define (make-accumulator x)
  (λ (y) (set! x (+ x y)) x))

;; 3.2 write a invocation-counting decorator
(define (make-monitored f)
  (let ([count 0])
    (λ (x)
      (cond
       [(equal? x 'how-many-calls?) count]
       [(equal? x 'reset-count) (set! count 0)]
       [else
        (set! count (+ 1 count))
        (f x)]))))

;; 3.3: add password protection to account object

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m p)
    (if (equal? password p)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request:
                 MAKE-ACCOUNT" m)))
        (error "Incorrect password")))
  dispatch)
