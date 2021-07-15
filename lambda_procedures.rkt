
; -------------------------------------------

; higher order procedures

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; -------------------------------------------

; procedure to find roots of equation

#|

Algorithm: half-interval-method

let f be an equation whose root we are trying to find

then take two points such that f(a) < 0 < f(b).


let x = average(a,b)
if f(x) > 0 => use next points (x,b)
if f(x) < 0 => use next points (b,x)

|#

(define (close-enough? x y) (< (abs (- x y)) 0.00001))
(define (average x y) (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (let (
        (midpoint (average neg-point pos-point))
        )
        (if (close-enough? neg-point pos-point)
          midpoint
          (let (
                (test-value (f midpoint))
                )
            (cond ((positive? test-value)
                    (search f neg-point midpoint)
                   )
                  ((negative? test-value)
                    (search f midpoint pos-point)
                   )
                  (else midpoint))))))

(define (half-interval-method f a b)
  (let (
        (a-value (f a))
        (b-value (f b))
        )
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b)
          )
          ((and (negative? b-value) (positive? a-value))
           (search f b a)
          )
          (else (error "aaaa")))))

; -------------------------------------------

; procedure to find fixed point of a function

#|
  Algorithm: average damping
|#

(define tolerance 0.000001)

 (define (fixed-point f first-guess)
   (define (close-enough? v1 v2)
     (< (abs (- v1 v2)) tolerance))
     (define (try guess)
       (display guess)
       (newline)
       (let ((next (f guess)))
         (if (close-enough? guess next)
             next
             (try next))))
     (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))


(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (sqr y))))
  1.0))

; -------------------------------------------

; (fixed-point (lambda (x) (/ (log 1000) (log x))) 10)
; (fixed-point (lambda (x) (average (/ (log 1000) (log x)) x)) 10)


; -------------------------------------------

; NEWTON's METHOD

#|

g => continuous differentiable function
root of g(x) = 0 where x is the fixed-point of continuous function f

f(x) = x - g(x)/g'(x)

        g(x + dx) - g(x)
 g'(x) = ---------------
              dx

if g(x) = 0 => f(x) = x - 0/g'(x)

∴ the fixed-point of function f is at x.

|#


(define dx 0.000001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
 (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
