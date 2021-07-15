
; -------------------------------------------

; helper procedure
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (cube x) (* x x x))
(define (indentity x) x)

;Higher order funtion

; linearly recursive procedure
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

; iterative procedure
(define (sumI term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; PRODUCT higher order function

; recursive procedure
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

; Iterative procedure

(define (productI term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

; Accumulate higher order procedure

; recursive procedure
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-Acc term a next b)
  (accumulate + 0 term a next b))

; -------------------------------------------

; usage of higher order function

(define (sum-cubes a b)
  (sum-Acc cube a inc b))

(define (sum-integers a b)
  (sumI indentity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (factorial b)
  (productI indentity 1 inc b))

; Simpson's Rule for integration.

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (n1 x) (+ x (* h 2)))
  (define (eq x) (/ (* h x) 3.0))

  (eq
  (+  (* (sumI f (n1 a) n1 b) 2)
      (* (sumI f (+ a h) n1 b) 4) a)))

; -------------------------------------------


