#lang racket

;; first, an op table with put and get

(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type)))

;; 2.73: convert deriv from section 2.3 to do data-directed style
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
           (if (same-variable? exp var)
               1
               0))
         (else ((get 'deriv (operator exp))
                (operands exp)
                var))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))


(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(put '(deriv +) (λ (x y)))
