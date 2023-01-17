#lang racket

(provide ->>)
(define [->> . xs]
  (define [go . fs]
    (cond [(null? fs) (error "Expected atleast one function.")]
          [(null? (cdr fs)) (car fs)]
          [else (λ [x] ((apply go (cdr fs))   
                        ((car fs) x)))]))
  (apply go (reverse xs)))

(provide bool)
(define [bool x y p] (if p y x))

(provide tagged-list?)
(define [tagged-list? exp tag]
  (if [pair? exp]
      (eq? (car exp) tag)
      #f))

(provide define-tagged-exp)
(define-syntax define-tagged-exp
  (syntax-rules ()
    ([ _ name tag] (define [name exp] (tagged-list? exp tag)))))

(provide not-implemented)
(define-syntax not-implemented
  (syntax-rules ()
    ([ _ name] (error "Not implemented: " name))))

(provide elem?)
(define (elem? x xs)
  (cond [(null? xs) #f]
        [(eq? (car xs) x) #t]
        [else (elem? x (cdr xs))]))

(provide const)
(define [const a b] a) 


(provide zip)
(define [zip xs ys]
  (cond [(null? xs) null]
        [(null? ys) null]
        [else (cons (cons (car xs) (car ys))
                    (zip (cdr xs) (cdr ys)))]))

(provide invalid-msg)
(define-syntax invalid-msg
  (syntax-rules ()
    ([_ n m] (error (~a "Not a valid message:") (symbol->string (quote n)) m))))


(provide seperate)
(define [seperate f xs]
  (if [null? xs]
      (list null null)
      (let ([r (seperate f (cdr xs))]
            [x (car xs)])
        (if [f x]
            (list (cons x (car r))
                  (cadr r))
            (list (car r)
                  (cons x (cadr r)))))))

(provide dispatch)
(define-syntax dispatch
  (syntax-rules ()
    ([_ p dispatcher] (define p (dispatcher (quote p))))))

