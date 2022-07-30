#lang racket

(require racket/format)	
(require rackunit)	

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                   (string-append "Withdrawn amount: "
                                   (number->string amount)
                                   ", remaining amount: "
                                   (number->string balance)
                                   ))
           "Insufficient funds"))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
                (string-append "Withdrawn amount: "
                                (number->string amount)
                                ", remaining amount: "
                                (number->string balance)
                                )
                )
        "Insufficient funds"))

  (define (deposit amount)
    (begin (set! balance (+ balance amount))
            (string-append "Desposited amount"
                            ", Current Balance: "
                            (number->string balance))))

  (define (view)
    (string-append "[View] Current Balance: " (number->string balance)))

  (define (dispatch m)
    (cond ((eq? m 'deposit) deposit)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'view) view)))
  dispatch)

(define w1 (make-account 100)
