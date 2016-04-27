#lang racket

;; 3.1, write a accumulator closure generator
(define (make-accumulator x)
  (λ (y) (set! x (+ x y)) x))
