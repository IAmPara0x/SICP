#lang racket

(require racket/format)
(require rackunit)

;; 3.1
(define (make-accumulator amount)
  (lambda (add-amount)
    (begin (set! amount (+ amount add-amount))
           amount
           )))

(define A (make-accumulator 5))

(check-equal? (A 10) 15)
(check-equal? (A 10) 25)

;; 3.2
(define (make-monitored f)

  (define counter 0)

  (define (how-many-calls?) (string-append "Current calls: " (number->string counter)))

  (define (reset-count)
    (begin (set! counter 0)
           "The counter has be set to 0."))

  (define (call m)
    (begin (set! counter (+ counter 1))
           (f m)))

  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) (how-many-calls?))
          ((eq? m 'reset-count) (reset-count))
          (else (call m))))
  dispatch)

(define s (make-monitored sqrt))

(check-equal? (s 100) 10)
(check-equal? (s 'how-many-calls?) "Current calls: 1")


;; 3.3
(define (make-account balance password)
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

  (define (dispatch pass m)
    (if (eq? password pass)
        (cond ((eq? m 'deposit) deposit)
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'view) view))
        (lambda (_) "incorrect password.")))
  dispatch)

(define acc (make-account 100 'secrect-pass))
