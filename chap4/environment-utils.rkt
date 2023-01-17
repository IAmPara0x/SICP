#lang racket

(require "utils.rkt")

(provide undefined-value)
(define undefined-value '*unassigned*)

(provide make-frame)
(define [make-frame variables values]  (make-hash (zip variables values)))

(provide the-empty-environment)
(define the-empty-environment  (list))

(provide first-frame)
(define first-frame car)

(provide enclosing-environment)
(define enclosing-environment cdr)

(provide frame-variables)
(define frame-variables hash-keys)

(provide frame-values)
(define frame-values hash-values)

(provide add-binding-to-frame!)
(define [add-binding-to-frame! var val frame]
  (hash-set! frame var val))

(provide extend-environment)
(define [extend-environment vars vals base-env]
  (cond [(= (length vars) (length vals))
         (cons (make-frame vars vals) base-env)]
        [(< (length vars) (length vals))
         (error "Too many arguments where provided: " vars vals)]
        [else
         (error "Too few arguments where provided: " vars vals)]))

(provide lookup-variable-value)
(define [lookup-variable-value var env]
  (cond [(eq? env the-empty-environment)
         (error "Unbound variable " var)]
        [(hash-has-key? (first-frame env) var)
         (if [eq? (hash-ref (first-frame env) var) undefined-value]
             (error (~a "Trying to use " var " but it's value is undefined."))
             (hash-ref (first-frame env) var))
         ]
        [else
         (lookup-variable-value var (enclosing-environment env))]))

(provide set-variable-value!)
(define [set-variable-value! var val env]
  (cond [(eq? env the-empty-environment) (error "Unbound variable: SET! " var)]
        [(hash-has-key? (first-frame env) var)
         (hash-update! (first-frame env) var (λ [_] val))]
        [else (set-variable-value! var val (enclosing-environment env))]))

(provide define-variable!)
(define [define-variable! var val env]
  (cond [(hash-has-key? (first-frame env) var)
         (hash-update! (first-frame env) var (λ [_] val))
         ]
        [else
         (add-binding-to-frame! var val (first-frame env))]))
