#lang racket

(define one-plus
  (λ (n) (+ n 1)))

(define make-withdraw
  (λ (initial-amount)
     ((λ (balance)
         (λ (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount)) balance)
                "Insufficient funds"))
        ) initial-amount)))

(define w (make-withdraw 100))
