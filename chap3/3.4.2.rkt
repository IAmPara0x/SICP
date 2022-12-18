#lang sicp

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell) (the-mutex 'acquire)))
            (eq? m 'release) (clear! cell)))
    the-mutex))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell false) false)))

(define (clear! cell) (set-car! cell false))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define serial-account-id 0)

(define (make-account-and-serializer balance)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        (error "Insufficient funds.")))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((account-serializer (make-serializer))
        (account-id serial-account-id)
        )

    (set! serial-account-id (+ serial-account-id 1))

    (define (dispatch m)
      (cond ((eq? m 'withdraw)   withdraw)
            ((eq? m 'deposit)    deposit)
            ((eq? m 'balance)    balance)
            ((eq? m 'serializer) account-serializer)
            ((eq? m 'account-id) account-id)
            (else (error "Unknown request: Make-Account " m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (withdraw account amount)
  (let ((s (account 'serializer))
        (w (account 'withdraw)))
    ((s w) amount)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))

    ((account1 'withdraw) difference)
    ((account2 'deposit)  difference)))

(define (serialized-exchange account1 account2)
  (let ((s1 (account1 'serializer))
        (s2 (account2 'serializer)))

    (if (< (account1 'account-id) (account2 'account-id))
        ((s1 (s2 exchange)) account1 account2)
        ((s2 (s1 exchange)) account1 account2))))

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

#|

First Name DB, Last Name DB, Full Name DB


user changes first name :: serialized-change-first-name

1. get-serialized-last-name
2. set-serialized-full-name

user changes last name :: serialized-change-last-name

1. get-serialized-first-name
2. set-serialized-full-name

Last Name DB -> Full Name DB
First Name DB -> Full Name DB

|#
