#lang racket

(require rackunit)

;; 3.7
(define (make-account balance correct-password)
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

  (define (check-pass pass) (eq? correct-password pass))

  (define (dispatch pass action)
        (cond ((not (eq? correct-password pass)) (lambda (_) "incorrect password."))
              ((eq? action 'deposit) deposit)
              ((eq? action 'withdraw) withdraw)
              ((eq? action 'view) view)
              ((eq? action 'check-pass) check-pass)
              (else "Invalid action")

              ))
  dispatch)

(define acc (make-account 100 'secrect-pass))

(check-equal? ((acc 'secrect-pass 'withdraw) 10) "Withdrawn amount: 10, remaining amount: 90")
(check-equal? ((acc 'secrect-pa 'withdraw) 10) "incorrect password.")

(define (make-joint acc original-pass joint-pass)

  (define (joint-acc-dispatch pass action)
    (cond ((eq? pass original-pass) (acc original-pass action))
          ((eq? pass joint-pass) (acc original-pass action))
          (else "incorrect password")))

  (if ((acc original-pass 'check-pass) original-pass)
      joint-acc-dispatch
      "Invalid password"))

(define paul-acc (make-account 1000 'paul))
(define peter-acc (make-joint paul-acc 'paul 'peter))

;; Unit tests
(check-equal? ((paul-acc 'paul 'withdraw) 10) "Withdrawn amount: 10, remaining amount: 990")
(check-equal? ((paul-acc 'peter 'withdraw) 10) "incorrect password.")
(check-equal? ((peter-acc 'peter 'withdraw) 10) "Withdrawn amount: 10, remaining amount: 980")
(check-equal? ((paul-acc 'paul 'view)) "[View] Current Balance: 980")


;; 3.8
(define f
  (let ((n 1) (tmp 0))
    (lambda (new-n)
      (begin (set! tmp (* n new-n))
             (set! n new-n)
            tmp))))

