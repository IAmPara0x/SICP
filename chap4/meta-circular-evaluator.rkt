#lang racket

(require "utils.rkt")

(provide EVALUATOR)
(define [EVALUATOR]

  ;; Utility procedures

  (define [type-tag exp]
    (cond [((->> not pair?) exp) #f]
          [(elem? (car exp) (all-tags)) (car exp)]
          [else null]))
  
  ;; Data structures
  (define *compound-exp-table* (make-hash))
  (define *atomic-exp-map* (list))

  (define [all-tags] (hash-keys *compound-exp-table*))

  (define [insert-compound-exp-evaluator tag proc]
    (begin (hash-set! *compound-exp-table* tag proc)
           ))

  (define [get-compound-exp-evaluator tag]
    (hash-ref *compound-exp-table* tag))

  (define [insert-atomic-exp pred? proc]
    (begin
      (set! *atomic-exp-map*
            (cons (cons pred? proc) *atomic-exp-map*))
      'ok))

  (define [eval-atomic-exp atomic-exp-evaluators exp env]
    (cond [(null? atomic-exp-evaluators)
           (error "Not a atomic expression" exp)]
          [((caar atomic-exp-evaluators) exp)
           ((cdar atomic-exp-evaluators) exp env)]
          [else
           (eval-atomic-exp (cdr atomic-exp-evaluators) exp env)]))

  (define [eval exp env]
    (cond [(pair? exp)
           ((get-compound-exp-evaluator (type-tag exp)) exp env)
           ]
          [else (eval-atomic-exp *atomic-exp-map* exp env)]))

  (define [dispatch m]
    (cond [(eq? m 'insert-compound-exp-evaluator) insert-compound-exp-evaluator]
          [(eq? m 'get-compound-exp-evaluator) get-compound-exp-evaluator]
          [(eq? m 'insert-atomic-exp) insert-atomic-exp]
          [(eq? m 'eval) eval]
          [else (error "Not a valid dispatch message: " m)]))

  dispatch)

(define eval-main (EVALUATOR))

(provide install-compound-exp-evaluator)
(define install-compound-exp-evaluator (eval-main 'insert-compound-exp-evaluator))

(provide get-compound-exp-evaluator)
(define get-compound-exp-evaluator (eval-main 'get-compound-exp-evaluator))

(provide install-atomic-exp-evaluator)
(define install-atomic-exp-evaluator (eval-main 'insert-atomic-exp))

(provide eval)
(define eval (eval-main 'eval))

(provide install-exp)
(define [install-exp m dispatcher]
  (cond [(eq? m 'atomic) (install-atomic-exp-evaluator (dispatcher 'pred)
                                                       (dispatcher 'evaluator))]
        [(eq? m 'compound) (install-compound-exp-evaluator (dispatcher 'tag)
                                                           (dispatcher 'evaluator))]
        [else (invalid-msg install-exp m)]))
