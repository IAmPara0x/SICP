
; Data-directed programming

#|


|#

; Converting deriv procedures created in symbolic_differentiation.rkt file into Data-directed programming

; Operation type table
(define *the-table* (make-hash))
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value))
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f))

; -------------------------------------------
;Helper functions

(define variable? symbol?)
(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define operator cadr)
(define (operands x) x)

; -------------------------------------------

; Precedence Algorithm

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
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))

        (else ((get 'deriv (operator exp)) (operands exp) var))))



; -------------------------------------------
; procedures for computing derivatives of particular expressions
; Data-directed programming

(define (install-deriv-sum)

  (define addend car)
  (define augend caddr)

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
            (+ a1 a2))
          (else (list a1 '+ a2))))

  (define (deriv-sum expr var)
    (make-sum (deriv (addend expr) var)
              (deriv (augend expr) var))
    )
  (put 'deriv '+ deriv-sum))

(define (install-deriv-product)

  (define multiplier car)
  (define multiplicand caddr)

  (define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
          ((=number? a1 1) a2)
          ((=number? a2 1) a1)
          ((and (number? a1) (number? a2))
            (* a1 a2))
          (else (list a1 '* a2))))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
            (+ a1 a2))
          (else (list a1 '+ a2))))

  (define (deriv-product expr var)
    (make-sum
      (make-product (multiplier expr) (deriv (multiplicand expr) var))
      (make-product (multiplicand expr) (deriv (multiplier expr) var))))

  (put 'deriv '* deriv-product))

(install-deriv-sum)
(install-deriv-product)

(define (derivV2 exp var) (deriv (precedence-sort exp) var))


(derivV2 '(x * x) 'x)
