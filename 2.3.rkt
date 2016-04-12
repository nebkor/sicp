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
