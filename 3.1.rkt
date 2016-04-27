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

;; 3.3/3.4: add password protection to account object, then check if the
;; wrong password has been given more than 6 times, at which point, call
;; the cops.
(define (make-account balance password)
  (let ([pcount 0])
    (define (withdraw amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m p)
      (if (equal? password p)
          (begin
            (set! pcount 0)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request:
                 MAKE-ACCOUNT" m))))
          (begin
            (set! pcount (+ 1 pcount))
            (when (> pcount 7)
              (call-the-cops))
            (error "Incorrect password"))))
    dispatch))

(define (call-the-cops)
  (displayln "COPS ARE COMING"))
