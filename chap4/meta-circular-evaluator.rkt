#lang racket

(require "utils.rkt")


(provide EVALUATOR)
(define [EVALUATOR]

  ;; Utility procedures

  (define [type-tag exp all-tags]
    (cond [((->> not pair?) exp) #f]
          [(elem? (car exp) all-tags) (car exp)]
          [else null]))
  
  ;; Data structures that contains keys the compound expressions tags
  ;; and value as functions that evaluate those expressions
  (define *compound-exp-eval-table* (make-hash))
  (define *atomic-exp-eval-map* (list))

  (define [all-exp-eval-table-tags] (hash-keys *compound-exp-eval-table*))

  (define [insert-compound-exp-evaluator tag proc]
    (begin (hash-set! *compound-exp-eval-table* tag proc)))

  (define [insert-atomic-exp-evaluator pred? proc]
    (begin
      (set! *atomic-exp-eval-map*
            (cons (cons pred? proc) *atomic-exp-eval-map*))
      'ok))

  (define [get-compound-exp-evaluator tag]
    (hash-ref *compound-exp-eval-table* tag))

  (define [eval-atomic-exp atomic-exp-evaluators exp env]
    (cond [(null? atomic-exp-evaluators)
           (error "Not a atomic expression" exp)]
          [((caar atomic-exp-evaluators) exp)
           ((cdar atomic-exp-evaluators) exp env)]
          [else
           (eval-atomic-exp (cdr atomic-exp-evaluators) exp env)]))

  (define [eval exp env]
    (cond [(pair? exp)
           ((get-compound-exp-evaluator (type-tag exp (all-exp-eval-table-tags))) exp env)]
          [else (eval-atomic-exp *atomic-exp-eval-map* exp env)]))

  ;; This table is for the analyzers of the compound expressions
  ;; every key contains a function as value that takes the expression as
  ;; input as returns a function that takes an env can perform some operations
  ;; on it, like modify the env.
  (define *compound-exp-analyzer-table* (make-hash))
  (define *atomic-exp-analyzer-map* (list))

  (define [get-compound-exp-analyzer tag]
    (hash-ref *compound-exp-analyzer-table* tag))

  (define [insert-compound-exp-analyzer tag proc]
    (begin (hash-set! *compound-exp-analyzer-table* tag proc)))

  (define [insert-atomic-exp-analyzer pred? proc]
    (begin
      (set! *atomic-exp-analyzer-map*
            (cons (cons pred? proc) *atomic-exp-analyzer-map*))
      'ok))

  (define [all-exp-analyzer-table-tags] (hash-keys *compound-exp-analyzer-table*))

  (define [analyze-atomic-exp atomic-exp-analyzer exp]
    (cond [(null? atomic-exp-analyzer)
           (error "Not a atomic expression" exp)]
          [((caar atomic-exp-analyzer) exp)
           ((cdar atomic-exp-analyzer) exp)]
          [else
           (analyze-atomic-exp (cdr atomic-exp-analyzer) exp)]))

  (define [analyze exp]
    (cond [(pair? exp)
           ((get-compound-exp-analyzer (type-tag exp (all-exp-analyzer-table-tags))) exp)]
          [else (analyze-atomic-exp *atomic-exp-analyzer-map* exp)]))

  (define [dispatch m]
    (cond [(eq? m 'insert-compound-exp-evaluator) insert-compound-exp-evaluator]
          [(eq? m 'get-compound-exp-evaluator) get-compound-exp-evaluator]
          [(eq? m 'insert-atomic-exp-evaluator) insert-atomic-exp-evaluator]
          [(eq? m 'eval) eval]
          [(eq? m 'analyze) analyze]
          [(eq? m 'insert-atomic-exp-analyzer) insert-atomic-exp-analyzer]
          [(eq? m 'insert-compound-exp-analyzer) insert-compound-exp-analyzer]
          [else (error "Not a valid dispatch message: " m)]))

  dispatch)

(define eval-main (EVALUATOR))

(provide install-atomic-exp-evaluator)
(define install-atomic-exp-evaluator (eval-main 'insert-atomic-exp-evaluator))

(provide install-compound-exp-evaluator)
(define install-compound-exp-evaluator (eval-main 'insert-compound-exp-evaluator))

(provide get-compound-exp-evaluator)
(define get-compound-exp-evaluator (eval-main 'get-compound-exp-evaluator))

(provide eval)
(define eval (eval-main 'eval))

(provide install-atomic-exp-analyzer)
(define install-atomic-exp-analyzer (eval-main 'insert-atomic-exp-analyzer))

(provide install-compound-exp-analyzer)
(define install-compound-exp-analyzer (eval-main 'insert-compound-exp-analyzer))

(provide analyze)
(define analyze (eval-main 'analyze))

(provide install-exp)
(define [install-exp m dispatcher]
  (cond [(eq? m 'atomic) (begin (install-atomic-exp-evaluator (dispatcher 'pred) (dispatcher 'evaluator))
                                (install-atomic-exp-analyzer (dispatcher 'pred) (dispatcher 'analyzer))
                                )]

        [(eq? m 'compound) (begin (install-compound-exp-evaluator (dispatcher 'tag) (dispatcher 'evaluator))
                                  (install-compound-exp-analyzer (dispatcher 'tag) (dispatcher 'analyzer))
                             )]
        [else (invalid-msg install-exp m)]))
