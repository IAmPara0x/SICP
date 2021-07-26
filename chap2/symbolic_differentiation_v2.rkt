
; -------------------------------------------

#|
Data Abstraction for differentiating using algebraic notation.
|#


(define variable? symbol?)
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
          (+ a1 a2))
        (else (list a1 '+ a2))))


(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2))
          (* a1 a2))
        (else (list a1 '* a2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define addend car)
(define augend caddr)

(define multiplier car)
(define multiplicand caddr)

; -------------------------------------------

#|

procedures for deciding precedence of operations.


Algorithm

 if exp has more that two operations (eg: x + w * y this exp has two operations + and *.) 
   then do following
   get the first element such that
    op1 w op2
   if op1 has higher precedence than op2 then
    (x op1 w) op2 y

   if op2 has higher precedence that op1 then
    x op1 (w op2 y)

   if op2 and op1 has same precedence then
    (x op1 w) op2 y

   repeat this process until there is only one operation left. Which in this case will be

    x + (w * y)

    since the current exp has only one operation ie + with x and (w * y)
    we terminate
|#

(define 1st-elem car)
(define op cadr)
(define 2nd-elem caddr)
(define 2nd-op cadddr)

(define (precedence-table exp)
  (cond
    ((eq? '+ exp) 0)
    ((eq? '* exp) 1)))

(define (precedence-check cmp exp)
  (cmp (precedence-table (op exp)) (precedence-table (2nd-op exp))))

(define (exp-f exp f)
  (list (f (1st-elem exp)) (op exp) (f (2nd-elem exp))))

(define (precedence-sort exp)
  (cond
    ((not (pair? exp)) exp)

    ((= (length exp) 3)
      (exp-f exp precedence-sort))

    ((precedence-check > exp)
      (precedence-sort (cons (exp-f exp precedence-sort) (cdddr exp))))

    ((precedence-check < exp)
     (append (list (precedence-sort (1st-elem exp)) (op exp))
             (list (precedence-sort (cddr exp)))))

    ((precedence-check = exp)
     (precedence-sort (cons (exp-f exp precedence-sort) (cdddr exp))))))

; -------------------------------------------

(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var)))
    ((product? exp)
     (make-sum (make-product (deriv (multiplier exp) var) (multiplicand exp))
               (make-product (multiplier exp) (deriv (multiplicand exp) var))))

    (else (error "unknow expression."))))

(define (derivV2 exp var) (deriv (precedence-sort exp) var))

(precedence-sort '(x * y + z * ( m * n + q)))

