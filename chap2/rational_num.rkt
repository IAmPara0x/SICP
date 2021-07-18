
; -------------------------------------------

; Data Abstraction for rational numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
  (* (numer y) (denom x)))
  (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
  (* (numer y) (denom x)))
  (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
  (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
  (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
  (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (or (negative? n) (negative? d))
      (cons (- (abs (/ n g))) (abs (/ d g)))
      (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; -------------------------------------------
; Data Abstraction for line segment

(define (make-point x y) ; constructor
  (cons x y))

(define (x-point point) (car point)) ; selector
(define (y-point point) (cdr point)) ; selector

(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")")
)

(print-point (make-point 1 2))

(define (make-segment start-point end-point) ; constructor
  (cons start-point end-point))

(define (start-segment segment) ; selector
  (car segment))
(define (end-segment segment) ; selector
  (cdr segment))

(define (average x y) (/ (+ x y) 2))

(define (midpoint-segment segment)
  (cons
    (average (x-point (start-segment segment)) (x-point (end-segment segment)))
    (average (y-point (start-segment segment)) (y-point (end-segment segment)))))


; -------------------------------------------
; Data Abstraction for representing pair of non-negative numbers as integers

#|
let a,b be a pair of non-negative integers
then it can be stored as 2^a*3^b

x := 2^a*3^b

car x := count = 0
         while x % 2 == 0
           divide x by 2
           count += 1
cdr x := while x % 2 == 0
           divide x by 2
           count += 1
          then log(x)
|#

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (sqr (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (make-pair-int a b)
  (*(fast-expt 2 a) (fast-expt 3 b)))

(define (int-iter count pair-int)
  (if (even? pair-int)
      (int-iter (+ count 1) (/ pair-int 2.0))
      (cons count pair-int)))

(define (int-car pair-int)
  (car (int-iter 0 pair-int)))

(define (int-cdr pair-int)
  (log (cdr (int-iter 0 pair-int)) 3)
)

(int-car (make-pair-int 2 3))


; -------------------------------------------
; Church numerals
; NOTE: completed this.

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

#|
lambda (f)
  lambda (x)
    (f x)

((n f) x) -> x
|#

; -------------------------------------------
; Extended Exercise 2.1.4


(define (make-interval a b)
  (if (> a b)
      (cons b a)
      (cons a b)
    )
  )

(define upper-bound car)
(define upper-bound cdr)


(define (add-interval x y)
  (make-interval (+ lower-bound x) (lower-bound y)
                 (+ upper-bound x) (upper-bound y)))

(define (sub-interval x y)
  (make-interval (- lower-bound x) (lower-bound y)
                 (- upper-bound x) (upper-bound y)))

(define (mul-interval x y)
  (let (
        (p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))
        )
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))


