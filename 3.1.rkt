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

;; 3.5: use the provided monte-carlo procedure to do numerical integration
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ([range (- high low)])
    (+ low (* (random) range))))

(define (get-area lower-x lower-y upper-x upper-y)
  (* (- upper-x lower-x)
     (- upper-y lower-y)))

(define (estimate-integral pred lower-x lower-y upper-x upper-y trials)
  (let ([area (get-area lower-x lower-y upper-x upper-y)]
        [experiment (λ () (pred (random-in-range lower-x upper-x)
                                (random-in-range lower-y upper-y)))])
    (* area (monte-carlo trials experiment))))

;; 3.6 write something that allows a password-protected account to be
;; accessed with an alias and different password
(define (make-joint acc opw npw)
  (λ (m p)
    (if (equal? p npw)
        (acc m opw)
        (acc m (list (random) (gensym))))))

;; racket@3.1.rkt> (define foo (make-account 100 'foo))
;; racket@3.1.rkt> (define bar (make-joint foo 'foo 'bar))
;; racket@3.1.rkt> ((foo 'withdraw 'foo) 10)
;; 90
;; racket@3.1.rkt> ((bar 'withdraw 'bar) 0)
;; 90
;; racket@3.1.rkt> ((bar 'withdraw 'bar) 10)
;; 80
;; racket@3.1.rkt> ((foo 'withdraw 'foo) 0)
;; 80
;; racket@3.1.rkt> ((bar 'withdraw 'butts) 0)
;; Incorrect password
