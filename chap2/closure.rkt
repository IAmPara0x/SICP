
; -------------------------------------------

; Closure Property

#|

Closure means if elements of a set are closed under a certain operation.

cons has closure property because

cons is a procedure that takes two elements to create a pair.

let S be the set such that
cons x1 x2 is possible where x1,x2 ∈ S

then (cons x1 x2) ∈ S, ∵ cons (cons x1 x2) x3 is possible

hence cons has closure property. python list also satisfies closure property.

|#

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (last-pair x) (list-ref x (- (length x) 1)))

(define (reverse x)
  (if (null? (cdr x))
    x
    (append (reverse (cdr x)) (list (car x)))
    ))

(define (same-parity . x)
  (define (filter f x)
    (cond ((null? x) '())
          ((f (car x))
           (append (list (car x))  (filter f (cdr x))))
          (else (append '() (filter f (cdr x))))))
  (if (even? (car x))
      (filter even? x)
      (filter odd? x))
)

; Mapping

(define nil '())

; higher order procedure
(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (square-list x)
  (map (lambda (x) (* x x)) x))

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (sqr (car things)) answer))))
    (reverse (iter items nil)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)))
              (+ (count-leaves (cdr x))))))


(define (deep-reverse x)
  (cond
    ((null? x) x)
    ((pair? (car x))
     (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))
    (else (append (deep-reverse (cdr x)) (list (car x))))))

(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else
          (append (fringe (car x)) (fringe (cdr x))))))


(define x (list (list 1 2) (list 3 4)))
