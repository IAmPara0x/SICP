#lang racket

(require "utils.rkt")
(require "meta-circular-evaluator.rkt")
(require "environment-utils.rkt")

;; Ex: 4.1 
;; (define [list-of-values-rl exps env]
  
;;   (if [no-operands? exps]
;;       '()
;;       (let ([rest-op (list-of-values (rest-operands exps) env)])
;;         (cons (eval (first-operand exps) env) 
;;               rest-op))))



(define [true? x] (not (false? x)))
(define [false? x] (eq? x #f))
(define [make-procedure parameters body env]
  (list 'procedure parameters body env))
(define [compound-procedure? p] (tagged-list? p 'procedure))
(define apply-premitive-procedure apply)
(define premitive-procedure? (->> not compound-procedure?))
(define procedure-parameters cadr)
(define procedure-body caddr)
(define procedure-environment cadddr)

(define [self-evaluating-exp]

  (define [self-evaluating? exp]
    (cond [(number? exp) #t]
          [(string? exp) #t]
          [else #f]))

  (define [dispatch m]
    (cond [(eq? m 'pred ) self-evaluating?]
          [(eq? m 'evaluator ) const]
          [else (invalid-msg self-evaluating-exp m)]))
  dispatch)

(define self-evaluating-exp-dispatcher [self-evaluating-exp])
[install-exp 'atomic self-evaluating-exp-dispatcher]

(define [variable-name-exp]
  (define variable? symbol?)

  (define [dispatch m]
    (cond [(eq? m 'variable?) variable?]
          [(eq? m 'pred) variable?]
          [(eq? m 'evaluator) lookup-variable-value]
          [else (invalid-msg variable-name-exp m)]))
  dispatch)

(define variable-name-exp-dispatcher [variable-name-exp])
(define variable? (variable-name-exp-dispatcher 'variable?))
[install-exp 'atomic variable-name-exp-dispatcher]

;; Quotation
;; (quote exp)
(define [quoted-exp]
  (define [text-of-quotation exp env] (cadr exp))

  (install-compound-exp-evaluator 'quote text-of-quotation)
  (define [dispatch m]
    (cond [(eq? m 'tag) 'quote]
          [(eq? m 'evaluator) text-of-quotation]
          [else (invalid-msg quoted-exp m)]))
  dispatch)

(define quoted-exp-dispatcher [quoted-exp])
[install-exp 'compound quoted-exp-dispatcher]

;; Assignment
;; (set! exp value)
(define [assignment-exp]
  (define assignment-variable cadr)
  (define assignment-value caddr)

  (define [eval-assignment exp env]
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)
  (define [dispatch m]
    (cond [(eq? m 'tag) 'set!]
          [(eq? m 'evaluator) eval-assignment]
          [else (invalid-msg assignment-exp m)]))
  dispatch)

(define assignment-exp-dispatcher [assignment-exp])
[install-exp 'compound assignment-exp-dispatcher]

;; Definition
;; (define [name params] body)
;; or
;; (define name value)
(define [definition-exp]


  (define [definition-variable exp]
    (if [variable? (cadr exp)]
        (cadr exp)
        (caadr exp)))

  (define [definition-value exp]
    (if [(->> symbol? cadr) exp]
        (caddr exp)
        (make-lambda ((cdadr) exp)
                     (cddr exp))))

  (define [eval-definition exp env]
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
                        env)
    'ok)
  (define [dispatch m]
    (cond [(eq? m 'tag) 'define]
          [(eq? m 'evaluator) eval-definition]
          [else (invalid-msg definition-exp m)]))
  dispatch)

(define definition-exp-dispatcher [definition-exp])
[install-exp 'compound definition-exp-dispatcher]

#|
Lambdas
(lambda [params] (body))
|#
(define [lambda-exp]

  (define lambda-parameters cadr)
  (define lambda-body cddr)


  (define [eval-lambda exp env]
    (make-procedure (lambda-parameters exp)
                    (cons 'begin (lambda-body exp))
                    env))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'lambda]
          [(eq? m 'evaluator) eval-lambda]
          [else (invalid-msg lambda-exp m)]))

  dispatch)

(define lambda-exp-dispatcher [lambda-exp])
[install-exp 'compound lambda-exp-dispatcher]

(define [make-lambda parameters body]
  (cons 'lambda (cons parameters body)))

;; If expression
;; (if (exp) exp exp)
(define [if-exp]

  (define if-predicate cadr)
  (define if-consequent caddr)
  (define [if-alternative exp]
    (if [(->> not null? cadddr) exp]
        ((->> cadddr) exp)
        'false))

  (define [eval-if exp env]
    
    (if [true? (eval (if-predicate exp) env)]
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'if]
          [(eq? m 'evaluator) eval-if]
          [else (invalid-msg if-exp m)]))

  dispatch)

(define if-exp-dispatcher [if-exp])
[install-exp 'compound if-exp-dispatcher]

(define [make-if pred t f] (list 'if pred t f))

;; Begin Expressions
;; (begin exp exp ...)
(define [begin-exp]

  (define begin-actions cdr)
  (define last-exp? (->> null? cdr))
  (define first-exp car)
  (define rest-exps cdr)

  (define [eval-sequence exps env]
    (cond [(last-exp? exps) (eval (first-exp exps) env)]
          [else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env)]))

  (define [sequence->exp seq]
    (cond [(null? seq) seq]
          [(last-exp? seq) (first-exp seq)]
          [else (make-begin seq)]))

  (define (make-begin seq) (cons 'begin seq))

  (define [eval-begin exp env] (eval-sequence (cdr exp) env))

  (define [dispatch m]
    (cond [(eq? m 'sequence->exp) sequence->exp]
          [(eq? m 'eval-sequence) eval-sequence]
          [(eq? m 'tag) 'begin]
          [(eq? m 'evaluator) eval-begin]
          [else (invalid-msg being-exp m)]))

  dispatch)

(define begin-exp-dispatcher [begin-exp])
[install-exp 'compound begin-exp-dispatcher]
(define sequence->exp (begin-exp-dispatcher 'sequence->exp))
(define eval-sequence (begin-exp-dispatcher 'eval-sequence))

;; Cond expression
(define [cond-exp]

  (define [clause->if clause alt-action]
    (cond [(lambda-clause? clause) 
           (make-application (make-lambda (list 'x) ;; Params
                                          (list (make-if 'x
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

  (define [eval-cond exp env] (eval (cond->if exp) env))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'cond]
          [(eq? m 'evaluator) eval-cond]
          [else (invalid-msg cond-exp m)]))
  dispatch)

(define cond-exp-dispatcher [cond-exp])
[install-exp 'compound cond-exp-dispatcher]

;; Application expression
(define [application-exp]


  (define operator car)
  (define operands cdr)

  ;; expects input param as list of operands
  (define no-operands? null?)
  (define first-operand car)
  (define rest-operands cdr)

  (define [apply procedure arguments]
    (cond [(premitive-procedure? procedure)
           (apply-premitive-procedure procedure arguments)]
          [(compound-procedure? procedure)
           (eval-sequence
            (list (procedure-body procedure))
            (extend-environment (procedure-parameters procedure)
                                arguments
                                (procedure-environment procedure)))]
          [else (error "Unknown procedure type: APPLY" procedure)]))

  (define [list-of-values exps env]
    (if [no-operands? exps]
        '()
        (let ([first-op (eval (first-operand exps) env)])
            (cons first-op
                (list-of-values (rest-operands exps) env)))))

  (define [eval-application exp env]
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))

  (define [dispatch m]
    (cond [(eq? m 'tag) null]
          [(eq? m 'evaluator) eval-application]
          [else (invalid-msg application-exp m)]))
  dispatch)

(define application-exp-dispatcher [application-exp])
[install-exp 'compound application-exp-dispatcher]

(define [make-application operator . operands]
  (cons operator operands))

;; and expression
;; (and exp1 exp2 ...)

(define [and-exp]
  (define and-exps cdr)

  (define [eval-and exp env]
    (let ([exps (and-exps exp)])
      (cond [(null? exps) 'true]
            [else
             (make-if (car exps) (cons 'and (cdr exps)) 'false)])))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'and]
          [(eq? m 'evaluator) eval-and]
          [else (invalid-msg and-exp m)]))
  dispatch)

(define and-exp-dispatcher [and-exp])
[install-exp 'compound and-exp-dispatcher]

(define [or-exp]

  (define or-exps cdr)

  (define [eval-or exp env]
    (let ([exps (or-exps exp)])
      (cond [(null? exps) 'false]
            [else
             (make-if (car exps) 'true (cons 'or (cdr exps)) )])))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'or]
          [(eq? m 'evaluator) eval-or]
          [else (invalid-msg or-exp m)]))
  dispatch)

(define or-exp-dispatcher [or-exp])
[install-exp 'compound or-exp-dispatcher]

(define [let-exp]
  (define let-vars cadr)
  (define let-body cddr)

  (define [let-vars-names vars] (map car vars))
  (define [let-vars-value vars] (map cadr vars))

  (define [let->combination exp]
    (apply make-application
           (cons (make-lambda ((->> let-vars-names let-vars) exp)
                            ((->> let-body) exp))
                 ((->> let-vars-value let-vars) exp))))

  (define [eval-let exp env] (eval (let->combination exp) env))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'let]
          [(eq? m 'evaluator) eval-let]
          [else (invalid-msg let-exp m)]))
  dispatch)

(define let-exp-dispatcher [let-exp])
[install-exp 'compound let-exp-dispatcher]


(define primitives
  (list
   (cons 'print println)
   (cons 'list list)
   (cons 'null null)
   (cons 'cons cons)
   (cons 'car car)
   (cons 'cdr cdr)
   (cons '* *)
   (cons '+ +)
   (cons '- -)
   (cons '= =)
   (cons 'eq? eq?)
   (cons 'True true)
   (cons 'False false)
   ))

(provide primitive-frame)
(define primitive-frame
  (make-frame (map car primitives)
              (map cdr primitives)))

(provide base-env)
(define base-env (cons primitive-frame the-empty-environment))
