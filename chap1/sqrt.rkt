; -------------------------------------------

#|
Calculating square root (newton's method)
Equation: To get approximation of sqrt of x with guess y.
                y' = y/2 + (x/y)/2
|#
(define (sqrt x)

  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  (define (improve guess)
    (averageOf2 (/ x guess) guess))

  (define (averageOf2 x y)
    (/ (+ x y) 2))

  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00001))

  (sqrt-iter 1.0))

; -------------------------------------------

#|
Calculating cube root (newton's method)
Equation: To get better approximation of cube root of x with guess y.
                y' = (x/y^2)/3 + 2y/3
|#

(define (cuberoot x)

  (define (two-times x) (* 2 x))

  (define (one-third x) (/ x 3))

  (define (cube x) (* x x x))

  (define (good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.00001))

  (define (improve guess x)
    (one-third (+ (/ x (square guess)) (two-times guess))))

  (define (cuberoot-iter guess x)
    (if (good-enough? guess x)
        guess
        (cuberoot-iter (improve guess x) x)))

  (cuberoot-iter 1.0 x))

; -------------------------------------------

