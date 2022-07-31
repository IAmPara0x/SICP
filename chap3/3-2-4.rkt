#lang racket

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)

  (define good-enough?
    (λ (guess) (< (abs ( - (sqr guess) x)) 0.001)))

  (define improve
    (λ (guess) (average guess (/ x guess))))

  (define sqrt-iter
    (λ (guess)
       (if (good-enough? guess)
           guess
           (sqrt-iter (improve guess)))))

  (sqrt-iter 1.0))
