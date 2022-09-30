#lang racket

(define (odd-or-even)
  (define (is-even? n)
    (if (= (remainder n 2) 0) #t #f))

  (define (is-odd? n)
    (not (is-even? n)))

  (define (dispatch m)
    (cond ((eq? m 'even) is-even?)
          ((eq? m 'odd) is-odd?)
          (else (error "Not a valid message."))))
  dispatch)

(define (cons/new x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined symbol"))
          ))
  dispatch)

