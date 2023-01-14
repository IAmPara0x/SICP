#lang racket

(require "utils.rkt")

;; Ex: 4.1 
;; (define [list-of-values-rl exps env]
  
;;   (if [no-operands? exps]
;;       '()
;;       (let ([rest-op (list-of-values (rest-operands exps) env)])
;;         (cons (eval (first-operand exps) env) 
;;               rest-op))))

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
          [(((->> car car) atomic-exp-evaluators) exp)
           (((->> cdr car) atomic-exp-evaluators) exp env)]
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
(define install-compound-exp-evaluator (eval-main 'insert-compound-exp-evaluator))
(define get-compound-exp-evaluator (eval-main 'get-compound-exp-evaluator))
(define install-atomic-exp-evaluator (eval-main 'insert-atomic-exp))

(provide eval)
(define eval (eval-main 'eval))

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

(define [make-frame variables values]  (make-hash (zip variables values)))
(define the-empty-environment  (list))
(define first-frame car)
(define enclosing-environment cdr)

(define frame-variables hash-keys)
(define frame-values hash-values)

(define [add-binding-to-frame! var val frame]
  (hash-set! frame var val))

(define [extend-environment vars vals base-env]
  (cond [(= (length vars) (length vals))
         (cons (make-frame vars vals) base-env)]
        [(< (length vars) (length vals))
         (error "Too many arguments where provided: " vars vals)]
        [else
         (error "Too few arguments where provided: " vars vals)]))

(define [lookup-variable-value var env]
  (cond [(eq? env the-empty-environment)
         (error "Unbound variable " var)]
        [(hash-has-key? (first-frame env) var)
         (hash-ref (first-frame env) var)]
        [else
         (lookup-variable-value var (enclosing-environment env))]))

(define [set-variable-value! var val env]
  (cond [(eq? env the-empty-environment) (error "Unbound variable: SET! " var)]
        [(hash-has-key? (first-frame env) var)
         (hash-update! (first-frame env) var (λ [_] val))]
        [else (set-variable-value! var val (enclosing-environment env))]))

(define [define-variable! var val env]
  (cond [(hash-has-key? (first-frame env) var)
         (hash-update! (first-frame env) var (λ [_] val))
         ]
        [else
         (add-binding-to-frame! var val (first-frame env))]))

(define [install-self-evaluating-exp]
  (define [self-evaluating? exp]
    (cond [(number? exp) #t]
          [(string? exp) #t]
          [else #f]))
  (install-atomic-exp-evaluator self-evaluating? const))

[install-self-evaluating-exp]

(define [install-variable-name-exp]
  (define variable? symbol?)

  (define [dispatch m]
    (cond [(eq? m 'variable?) variable?]
          [else (error "Unknown message: dispatch install-variable-name exp " m)]))

  (install-atomic-exp-evaluator symbol? lookup-variable-value)
  dispatch)

(define variable-name-exp-dispatcher [install-variable-name-exp])
(define variable? (variable-name-exp-dispatcher 'variable?))


;; Quotation
;; (quote exp)
(define [install-quoted-exp]
  (define [text-of-quotation exp env] ((->> car cdr) exp))
  (install-compound-exp-evaluator 'quote text-of-quotation))

;; Assignment
;; (set! exp value)
(define [install-assignment-exp]
  (define assignment-variable (->> car cdr))
  (define assignment-value (->> car cdr cdr))

  (define [eval-assignment exp env]
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)
  (install-compound-exp-evaluator 'set! eval-assignment))

;; Definition
;; (define [name params] body)
;; or
;; (define name value)
(define [install-definition-exp]


  (define [definition-variable exp]
    (if [variable? (cadr exp)]
        (cadr exp)
        (caadr exp)))

  (define [definition-value exp]
    (if [(->> symbol? cadr) exp]
        (caddr exp)
        (make-lambda ((->> cdr car cdr) exp)
                     (cddr exp))))

  (define [eval-definition exp env]
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
                        env)
    'ok)

  (install-compound-exp-evaluator 'define eval-definition))


#|
Lambdas
(lambda [params] (body))
|#
(define [install-lambda-exp]

  (define lambda-parameters (->> car cdr))
  (define lambda-body (->> cdr cdr))


  (define [eval-lambda exp env]
    (make-procedure (lambda-parameters exp)
                    (cons 'begin (lambda-body exp))
                    env)
    )

  (install-compound-exp-evaluator 'lambda eval-lambda))

(define [make-lambda parameters body]
  (cons 'lambda (cons parameters body)))

;; If expression
;; (if (exp) exp exp)
(define [install-if-exp]

  (define if-predicate (->> car cdr))
  (define if-consequent (->> car cdr cdr))
  (define [if-alternative exp]
    (if [(->> not null? car cdr cdr cdr) exp]
        ((->> car cdr cdr cdr) exp)
        'false))

  (define [eval-if exp env]
    
    (if [true? (eval (if-predicate exp) env)]
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

  (install-compound-exp-evaluator 'if eval-if))

(define [make-if pred t f] (list 'if pred t f))

;; Begin Expressions
;; (begin exp exp ...)
(define [install-begin-exp]

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
          [else (error "Unknown message: dispatch install-begin-exp " m)]))

  (install-compound-exp-evaluator 'begin eval-begin)
  dispatch)

(define begin-exp-dispatcher [install-begin-exp])
(define sequence->exp (begin-exp-dispatcher 'sequence->exp))
(define eval-sequence (begin-exp-dispatcher 'eval-sequence))

;; Cond expression
(define [install-cond-exp]

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

  (install-compound-exp-evaluator 'cond eval-cond)
)

;; Application expression
(define [install-application-exp]


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

  (install-compound-exp-evaluator null eval-application))

(define [make-application operator . operands]
  (cons operator operands))

;; and expression
;; (and exp1 exp2 ...)

(define [install-and-exp]

  (define and-exps cdr)

  (define [eval-and exp env]

    (let ([exps (and-exps exp)])
      (cond [(null? exps) 'true]
            [else
             (make-if (car exps) (cons 'and (cdr exps)) 'false)])))

  (install-compound-exp-evaluator 'and eval-and))

(define [install-or-exp]

  (define or-exps cdr)

  (define [eval-or exp env]

    (let ([exps (or-exps exp)])
      (cond [(null? exps) 'false]
            [else
             (make-if (car exps) 'true (cons 'or (cdr exps)) )])))

  (install-compound-exp-evaluator 'or eval-or))

(define [install-let-exp]
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

  (install-compound-exp-evaluator 'let eval-let))

[install-self-evaluating-exp ]
[install-variable-name-exp   ]
[install-quoted-exp          ]
[install-assignment-exp      ]
[install-definition-exp      ]
[install-lambda-exp          ]
[install-let-exp             ]
[install-if-exp              ]
[install-cond-exp            ]
[install-and-exp             ]
[install-or-exp              ]
[install-application-exp     ]

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


