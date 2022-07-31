#lang racket

(define square
  (λ (x) (* x x)))

(define factorial
  (λ (n) (fact-iter 1 1 n)))

(define fact-iter
  (λ (product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count))))
