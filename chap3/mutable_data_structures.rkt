#lang sicp

; -------------------------------------------
; Mutable version of append
(define (append! x y)
  (cond ((null? x) (error "empty pair"))
        ((pair? (cdr x)) (append! (cdr x) y))
        (else (set-cdr! x (cons y '())))))

; Mutable List

(define a (cons 1 (cons 2 '())))
(append! a 3)
(append! a 4)

; (define x (cons 'a 'b))
; (define z1 (cons x x))
; -------------------------------------------

; Ex 3.16

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
      (count-pairs (cdr x))
      1)))

(define r 'rem)
(define l1 (cons r r)) ; count-pairs 3
(define l2 (list l1)) ; count-pairs 4
(define l3 (list l1 r 'yuno)) ; count-pairs 7

; -------------------------------------------

; TODO: Ex 3.17

; -------------------------------------------

; Ex 3.18

#|

Main Idea: (eq? l_i, l) = True
            where, l is the given list
                  l_i is list from ith element onwards where i is the multiple of the length of the list.
|#

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; A basic algorithm the checks if the list is cycled
(define (check-cycle x)
  (define (check x1 y1 count)
    (cond ((= count 0) #f)
          ((eq? x1 y1) #t)
          (else (check (cdr x1) y1 (- count 1)))))

  (define (helper y count)
    (cond
      ((not (pair? y)) #f)
      ((check x (cdr y) count) #t)
      (else (helper (cdr y) (+ count 1)))))
  (helper x 0))

(define x '(1 2 3 4 5 6 7 8))
(define y '(1 2 3 4 5 6 7 8))
(set-cdr! (cdddr (cddddr y)) (cdddr y))
(define z '(1))
(set-cdr! z z)

(check-cycle x)
(check-cycle y)
(check-cycle z)

; -------------------------------------------
