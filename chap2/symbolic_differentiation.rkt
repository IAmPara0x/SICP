

; Developing system that performs differentiation of algebraic expression

(define variable? symbol?)

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
          (+ a1 a2))
        (else (list '+ a1 a2)))
)


(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2))
          (* a1 a2))
        (else (list '* a1 a2))))

(define (make-exponentiation x n)
  (cond ((=number? n 1) x)
        ((=number? n 0) 1)
        (else (list '^ x n))))


(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define addend cadr)

 (define (augend s)
   (if (null? (cdddr s))
       (caddr s)
       (cons '+ (cddr s))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define multiplier cadr)

(define (multiplicand p)
 (if (null? (cdddr p))
     (caddr p)
     (cons '* (cddr p))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '^)))
(define base cadr)
(define exponent caddr)


; Deriv version that works but doesn't follow rules of ex 2.57
#|
(define multiplicand cddr)
(define augend cddr)

(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp ) (if (same-variable? exp var) 1 0))
    ((sum? exp)
      (if (null? (augend exp))
        (make-sum (deriv (addend exp) var)
                                  0)
        (make-sum (deriv (addend exp) var)
                  (deriv (cons '+ (augend exp)) var)))
    )
    ((product? exp)
     (if (null? (multiplicand exp))
      (make-sum
        0
        (make-product (deriv (multiplier exp) var)
                      1))
        (make-sum
          (make-product (multiplier exp)
                        (deriv (cons '* (multiplicand exp)) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))
      )

    )
    ((exponentiation? exp)
      (if (same-variable? (base exp) var)
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1))
                        )
          0))
    (else (error "unknow expression."))))
|#

(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp ) (if (same-variable? exp var) 1 0))
    ((sum? exp)
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var)))
    ((product? exp)
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
    ((exponentiation? exp)
      (if (same-variable? (base exp) var)
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          0))
    (else (error "unknow expression."))))

(deriv '(* (* x y) (+ x 3)) 'x)
