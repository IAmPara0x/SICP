#lang sicp

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
              (if (test-and-set! cell)
              (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
      the-mutex))

(define (clear! cell) (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))

(define (semaphore n)
  (define (test-and-set! acc)
    (if (>= acc n)
        true
        (begin (set! acc (+ acc 1)) false)))
  (define (clear! acc) (set! acc (- acc 1)))

  (let ((acc 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! acc)
                 (the-semaphore 'acquire)))
            ((eq? m 'release)
              (clear! acc))))
    the-semaphore))


(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-account balance)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define protected (make-serializer))

  (define (dispatch m)
    (cond ((eq? m 'withdraw) (protected withdraw))
          ((eq? m 'deposit) (protected deposit))
          ((eq? m 'balance) balance)
          (else (error m))))
dispatch)

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))


(define (make-account-and-serializer balance)

  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount)))))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
        ((serializer1 (serializer2 exchange))
          account1
          account2)))

(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

(define x (make-account 1000))
