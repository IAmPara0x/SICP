#lang racket

(require rackunit)
(require r5rs)

(define nil '())

(define x (list (list 1 2) 3 4))
(define y (list 5 6))

(define (last-elem x)
  (if (null? (cdr x))
      x
      (last-elem (cdr x))
      ))

(define (append! x y)
  (begin (set-cdr! (last-elem x) y) x))


(append! x y)
(check-equal? x (list (list 1 2) 3 4 5 6))

(set! x nil)
(check-equal? x nil)

(define (make-cycle x)
  (begin (set-cdr! (last-elem x) x) x))

(define z (make-cycle (list 'a 'b 'c)))

#|

       x               y

(loop (list 1 2 3 4) '())

(loop (list 2 3 4)   '(list 1))

(loop (list 3 4)     '(list 2 1))

(loop (list 4)       '(list 3 2 1))

(loop (nil)         '(list 4 3 2 1))

|#

(define (mystery x)

   (define (loop x y)
      (if (null? x)
        y
        (let ((temp (cdr x)))
             (set-cdr! x y)
             (loop temp x)
             )))

  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
