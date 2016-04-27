#lang racket

;; 3.1, write a accumulator closure generator
(define (make-accumulator val)
  (let ([x val])
    (λ (y) (set! x (+ x y)) x)))
