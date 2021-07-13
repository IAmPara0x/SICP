; Sum of squares greater two of three numbers
(define (square x) (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x
   ))

(define (sumofsquare x y)
  (+ (square x) (square y))
  )

(define (greaterOfTwo x y)
  (if (< x y)
  y
  x))

(define (greaterSquare x y z)
  (if (< x y)
      (sumofsquare y (greaterOfTwo 1 x z))
      (sumofsquare x (greaterOfTwo 1 y z))
  ))

; -------------------------------------------

#|
  Ackerman function
|#
(define (A x y)
  (cond ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1) (A x (- y 1))))))

; -------------------------------------------

#|
Coin change problem.
|#

; Recursive Procedure

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else
          (+ (cc amount (- kinds-of-coins 1))
             (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
          ))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)))

; -------------------------------------------

#|
f(n) = n if n < 3
f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3
|#

;Recursive Procedure

(define (f1.11 n)
  (define (helper n x y)
    (* (f1.11 (- n x)) y))
  (cond ((< n 3) n)
        (else
          (+ (helper n 1 1) (helper n 2 2) (helper n 3 3)))))

;Iterative Procedure

(define (f1.11iter yp y x1 x2 x3)
  (define update (+ x1 (* x2 2) (* x3 3)))
  (if (= (- yp 1) y)
      x1
      (f1.11iter (+ yp 1) y update x1 x2))
)

(define (f1.11T x) (f1.11iter 3 x 2 1 0))

; -------------------------------------------

#|
  Sin x ~ x (if x is sufficiently small)
|#

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; -------------------------------------------

