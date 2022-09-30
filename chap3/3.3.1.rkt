#lang racket

(require rackunit)
(require r5rs)
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

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

;; 3.19 check if a lists contains a cycle.
(define (contains-cycle? x)

  (define (check x y n)
    (cond ((= n 0) (eq? x y))
          ((eq? x y) #t)
          (else (check (cdr x) y (- n 1)))
          ))

  (define (go y n)
    (cond ((null? y) #f)
          ((check x (cdr y) n) #t)
          (else (go (cdr y) (+ n 1)))
          ))

  (go x 0))

;; Tests

(define non-cyclic-list (mlist 'a 'b 'c 'd 'e 'f 'g))
(define cyclic-list (make-cycle (mlist 'a 'b 'c 'd 'e 'f 'g 'h)))

(define M (mcons 'u 'b))
(define N (mcons 'v 'c))
(define O (mcons 'w 'd))
(define P (mcons 'd 'e))
(define Q (mcons 'x 'f))
(define R (mcons 'y 'g))
(define S (mcons 'z 'h))

(set-mcdr! M N)
(set-mcdr! N O)
(set-mcdr! O P)
(set-mcdr! P M)
(set-mcar! P Q)
(set-mcdr! Q R)
(set-mcdr! R S)
(set-mcdr! S P)

(check-true  (contains-cycle? cyclic-list))
(check-false (contains-cycle? non-cyclic-list))

(set-mcdr! S '())
(check-true (contains-cycle? M))

(set-mcdr! P Q)
(check-false (contains-cycle? M))
