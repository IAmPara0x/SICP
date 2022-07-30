#lang racket

(require rackunit)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

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

(check-equal? (factorial 0) 2)
