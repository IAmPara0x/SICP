#lang racket

(require "utils.rkt")
(require rackunit)


;; Example for Ex 4.1
(define-syntax cons-rl
  (syntax-rules ()
    ((_ x xs) ((λ [y] (cons x y)) xs))))

(define [tag str exp]
  (begin (println str) exp))

(cons-rl (tag "first exp" 1) (tag "second exp" 2))


;; Expressions for the evaluator

;; Begin

(define-tagged-exp begin? 'begin)
(define begin-actions cdr)
(define last-exp? (->> null? cdr))
(define first-exp car)
(define rest-exp cdr)

(define [sequence->exp seq]
  (cond [(null? seq) seq]
        [(last-exp? seq) (first-exp seq)]
        [else (make-begin seq)]))

(define (make-begin seq) (cons 'begin seq))

;; Conditionals

(define if-exp '(if (eq? 1 1) 1 0))

(define-tagged-exp if? 'if)
(define if-predicate (->> car cdr))
(define if-consequent (->> car cdr cdr))
(define if-alternative (->> car cdr cdr cdr))
(define [make-if pred t f] (list 'if pred t f))

(check-true (if? if-exp))
(check-equal? (if-predicate if-exp) '(eq? 1 1))
(check-equal? (if-consequent if-exp) 1)
(check-equal? (if-alternative if-exp) 0)

(define cond-exp '(cond [(eq? 2 0) "clause 1"] [(eq? 2 1) "clause 2"] [else "else clause"]))
(define cond-lambda-exp '(cond ((begin (println "lol") (eq? 1 1)) => not) (else false)))

(define [clause->if clause alt-action]
  (cond [(lambda-clause? clause) 
                                 (make-application
                                  (make-lambda (list 'x) ;; Params
                                               (list 
                                                 (make-if 'x
                                                          (make-application ((->> car cond-actions) clause) 'x)
                                                          alt-action)))
                                  (cond-predicate clause))]
        [else (make-if (cond-predicate clause)
                       ((->> sequence->exp cond-actions) clause)
                       alt-action)]))

(define [expand-clauses clauses]
  (cond [(null? clauses) 'false]
        [((->> cond-else-clause? car) clauses)
         ((->> sequence->exp cond-actions car) clauses)]
        [((->> null? cdr) clauses)
         (clause->if (car clauses) 'false)]
        [else
         (clause->if (car clauses)
                     ((->> expand-clauses cdr) clauses))]))

(define cond-clauses cdr)
(define [cond-else-clause? clause] (eq? (cond-predicate clause) 'else))
(define cond-predicate car)
(define [cond-actions clause]
  (cond [(lambda-clause? clause)
         (cddr clause)]
        [else (cdr clause)]))
(define [lambda-clause? exp] (eq? (cadr exp) '=>) )
(define cond->if (->> expand-clauses cond-clauses))


(define-tagged-exp cond? 'cond)

;; Lambda Expression

(define lambda-exp '(lambda [t] (+ 1 t)))

(define-tagged-exp lambda? 'lambda)

(define lambda-parameters (->> car cdr))
(define lambda-body (->> car cdr cdr))


(check-true (lambda? lambda-exp))
(check-equal? (lambda-parameters lambda-exp) '[t])
(check-equal? (lambda-body lambda-exp) '(+ 1 t))


;; Assignment
(define-tagged-exp assignment? 'set!)
(define assignment-variable (->> car cdr))
(define assignment-value (->> car cdr cdr))

;; let expression

(define let-exp '(let [(a 1) (b 2) (c 3)]
                   (println "Let expression")
                   (+ a b c)
                   ))

(define [let->combination exp]
  (define let-vars cadr)
  (define let-body cddr)

  (define [let-vars-names vars] (map car vars))
  (define [let-vars-value vars] (map cadr vars))


  (apply make-application
         (cons (make-lambda ((->> let-vars-names let-vars) exp)
                            ((->> let-body) exp))
               ((->> let-vars-value let-vars) exp))))

#|

(let [(var1 <exp>)
      (var2 <exp>)
     ]
  (<exp>)
  (<exp>)
  (<exp>)
  (<exp>)
  (<exp>)
  )

|#


;; Application expression

(define-tagged-exp application? 'call)
(define operator (->> car cdr))
(define operands (->> cdr cdr))
(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define [make-application operator . operands]
  (cons operator operands))

(define [make-lambda parameters body]
  (cons 'lambda (cons parameters body)))

(define assignment-exp '(call + x 1))
(application? assignment-exp)
(operator assignment-exp)
(operands assignment-exp)


(define [EVALUATOR]

  ;; Utility procedures

  (define compound-procedure? pair?)

  (define [type-tag exp]
    (cond [((->> not pair?) exp) #f]
          [(elem? (car exp) all-tags) (car exp)]
          [else null]))
  
  ;; Data structures
  (define *compound-exp-table* (make-hash))
  (define *atomic-exp-map* (list))

  (define all-tags (hash-keys *compound-exp-table*))

  (define [insert-compound-exp-evaluator tag proc]
    (begin (hash-set! *compound-exp-table* (list tag) proc)
           (set! all-tags (cons tag all-tags))
           ))

  (define [get-compound-exp-evaluator tag]
    (hash-ref *compound-exp-table* (list tag)))

  (define [insert-atomic-exp pred? proc]
    (begin
      (set! *atomic-exp-map*
            (cons (cons pred? proc) *atomic-exp-map*))
      'ok))

  (define [eval-atomic-exp atomic-exp-evaluators exp env]
    (cond [(null? atomic-exp-evaluators)
           (error "Not a atomic expression" exp)]
          [(((->> car car) atomic-exp-evaluators) exp)
           (((->> cdr car) atomic-exp-evaluators) exp env)]
          [else
           (eval-atomic-exp (cdr atomic-exp-evaluators) exp env)]))

  (define [eval exp env]
    (cond [(compound-procedure? exp)
           ((get-compound-exp-evaluator (type-tag exp)) exp env)]
          [else (eval-atomic-exp *atomic-exp-map* exp env)]))

  (define [dispatch m]
    (cond [(eq? m 'insert-compound-exp-evaluator) insert-compound-exp-evaluator]
          [(eq? m 'get-compound-exp-evaluator) get-compound-exp-evaluator]
          [(eq? m 'insert-atomic-exp) insert-atomic-exp]
          [(eq? m 'eval) eval]
          [else (error "Not a valid dispatch message: " m)]))

  dispatch)

(define eval-main (EVALUATOR))
(define install-compound-exp-evaluator (eval-main 'insert-compound-exp-evaluator))
(define get-compound-exp-evaluator (eval-main 'get-compound-exp-evaluator))
(define install-atomic-exp-evaluator (eval-main 'insert-atomic-exp))
(define eval (eval-main 'eval))


(define [install-self-evaluating-exp]
  (define [self-evaluating? exp]
    (cond [(number? exp) #t]
          [(string? exp) #t]
          [else #f]))
  (install-atomic-exp-evaluator self-evaluating? const))

(define [install-variable-name-exp]
  (define [lookup-variable-value exp env] (error "lookup-variable-value not implemented."))
  (install-atomic-exp-evaluator symbol? lookup-variable-value))

(define [install-quoted-exp]
  (define [text-of-quotation exp env] ((->> car cdr) exp))
  (install-compound-exp-evaluator 'quote text-of-quotation))

;; (if pred a b)
;; (define [install-if-exp]

;;   (define if-predicate (->> car cdr))
;;   (define if-consequent (->> car cdr cdr))
;;   (define [if-alternative exp]
;;     (if [(->> not null? car cdr cdr cdr) exp]
;;         ((->> car cdr cdr cdr) exp)
;;         'false))

;;   (define [eval-if exp env]
;;     (if [true? (eval (if-predicate exp) env)]
;;         (eval (if-consequent exp) env)
;;         (eval (if-alternative exp) env)))

;;   (install-compound-exp-evaluator 'if eval-if))

;; (define [install-and-exp]

;;   (define and-exps cdr)

;;   (define [eval-and exp env]

;;     (let ([exps (and-exps exp)])
;;       (cond [(null? exps) 'true]
;;             [else
;;              (make-if (car exps) (cons 'and (cdr exps)) 'false)])))

;;   (install-compound-exp-eval 'and eval-and))


[install-self-evaluating-exp]
[install-variable-name-exp]
[install-quoted-exp]
;; [install-if-exp]
;; [install-and-exp]


(cond->if cond-lambda-exp)
(let->combination let-exp)



;; (define make-frame zip)

;; 
;; (define [frame-values frame] (map cadr frame))


;; (define-syntax add-binding-to-frame!
;;   (syntax-rules ()
;;     ((_ var val frame) (set! frame
;;                              (cons (list var val) frame)
;;                              ))))

;; (assoc "d" frame1)


