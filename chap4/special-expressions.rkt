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

(define TRUE 'True)
(define FALSE 'False)

(define [true? x] (eq? TRUE x))
(define [false? x] (eq? FALSE x))

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
          [(null? exp) #t]
          [(true? exp) #t]
          [(false? exp) #t]
          [(eq? exp undefined-value) #t]
          [else #f]))

  (define [analyze-self-evaluating-exp exp]
    (if [self-evaluating? exp]
        (λ [_env] exp)
        (error "Not a self-evaluating expression: " exp))
    )

  (define [dispatch m]
    (cond [(eq? m 'pred ) self-evaluating?]
          [(eq? m 'evaluator ) const]
          [(eq? m 'analyzer ) analyze-self-evaluating-exp]
          [else (invalid-msg self-evaluating-exp m)]))

  dispatch)

(define self-evaluating-exp-dispatcher [self-evaluating-exp])
[install-exp 'atomic self-evaluating-exp-dispatcher]

(define [variable-name-exp]

  (define [variable? exp]
    (and (not (eq? exp undefined-value))
         (not (eq? exp TRUE))
         (not (eq? exp FALSE))
         (symbol? exp)))

  (define [analyze-variable exp]
    (if [variable? exp]
        (λ [env] (lookup-variable-value exp env))
        (error "Not a variable: " exp)))

  (define [dispatch m]
    (cond [(eq? m 'pred) variable?]
          [(eq? m 'evaluator) lookup-variable-value]
          [(eq? m 'analyzer) analyze-variable]
          [else (invalid-msg variable-name-exp m)]))
  dispatch)

(define variable-name-exp-dispatcher [variable-name-exp])
(define variable? (variable-name-exp-dispatcher 'pred))
[install-exp 'atomic variable-name-exp-dispatcher]

;; Quotation
;; (quote exp)
(define [quoted-exp]

  (define [text-of-quotation exp env] (cadr exp))

  (define [quote-expression? exp] (eq? 'quote (car exp)))

  (define [analyze-quoted-exp exp]
    (if [quote-expression? exp]
        (λ [env] exp)
        (error "Not a quote expression: " exp)))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'quote]
          [(eq? m 'evaluator) text-of-quotation]
          [(eq? m 'analyzer) analyze-quoted-exp]
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

  (define [analyze-assignment exp]
    (let ([name (assignment-variable exp)]
          [value-proc (analyze (assignment-value exp))])

      (λ [env] (set-variable-value! name
                                    (value-proc env)
                                    env))))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'set!]
          [(eq? m 'evaluator) eval-assignment]
          [(eq? m 'analyzer) analyze-assignment]
          [else (invalid-msg assignment-exp m)]))
  dispatch)

(define [make-assignment exp val]
  (list 'set! exp val))

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
    (if [variable? (cadr exp)]
        (caddr exp)
        (make-lambda (cdadr exp)
                     (cddr exp))))

  (define [eval-definition exp env]
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
                        env)
    'ok)
  
  (define [definition? exp]
    (tagged-list? exp 'define ))

  (define [analyze-definition exp]
    (let ([name (definition-variable exp)]
          [value-proc (analyze (definition-value exp))])
      (λ [env] (define-variable! name (value-proc env) env))))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'define]
          [(eq? m 'definition?) definition?]
          [(eq? m 'definition-variable) definition-variable]
          [(eq? m 'definition-value) definition-value]
          [(eq? m 'evaluator) eval-definition]
          [(eq? m 'analyzer) analyze-definition]
          [else (invalid-msg definition-exp m)]))
  dispatch)

(define definition-exp-dispatcher [definition-exp])
(dispatch definition? definition-exp-dispatcher)
(dispatch definition-variable definition-exp-dispatcher)
(dispatch definition-value definition-exp-dispatcher)

[install-exp 'compound definition-exp-dispatcher]

#|
Lambdas
(lambda [params] body)
|#
(define [lambda-exp]

  (define lambda-parameters cadr)
  (define lambda-body cddr)

  (define [eval-lambda exp env]
    (let* ([result (seperate definition? (lambda-body exp))]
           [definitions (car result)]
           [expressions (cadr result)]
           [to->letvar (λ [d] (list (definition-variable d) undefined-value))]
           [to->set! (λ [d] (make-assignment (definition-variable d) (definition-value d)))]
           )

      (if [null? definitions]
          (make-procedure (lambda-parameters exp)
                          (cons 'begin (lambda-body exp))
                          env)

          (make-procedure (lambda-parameters exp)
                          (make-let (map to->letvar definitions)
                                    (append (map to->set! definitions) expressions))
                          env))))

  (define [analyze-lambda exp]
    (let* ([params (lambda-parameters exp)]
           [body-proc (lambda-body exp)]
           [result (seperate definition? (lambda-body exp))]
           [definitions (car result)]
           [expressions (cadr result)]
           [to->letvar (λ [d] (list (definition-variable d) undefined-value))]
           [to->set! (λ [d] (make-assignment (definition-variable d) (definition-value d)))])

      (if [null? definitions]
          (λ [env] (make-procedure params
                                   (analyze (cons 'begin body-proc))
                                   env))
          (λ [env] (make-procedure params
                                   (analyze (make-let (map to->letvar definitions)
                                                      (append (map to->set! definitions) expressions)))
                                   env)))))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'lambda]
          [(eq? m 'evaluator) eval-lambda]
          [(eq? m 'analyzer) analyze-lambda]
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

  (define [analyze-if exp]
    (let ([pred-proc (analyze (if-predicate exp))]
          [consequent-proc (analyze (if-consequent exp))]
          [alternative-proc (analyze (if-alternative exp))])

      (λ [env]
        (if (true? (pred-proc env))
            (consequent-proc env)
            (alternative-proc env)))))

  (define [eval-if exp env]
    (if [true? (eval (if-predicate exp) env)]
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'if]
          [(eq? m 'evaluator) eval-if]
          [(eq? m 'analyzer) analyze-if]
          [else (invalid-msg if-exp m)]))

  dispatch)

(define if-exp-dispatcher [if-exp])
[install-exp 'compound if-exp-dispatcher]

(define [make-if pred t f] (list 'if pred t f))

;; Begin Expressions
;; (begin exp exp ...)
(define [begin-exp]

  (define last-exp? (->> null? cdr))
  (define first-exp car)
  (define rest-exps cdr)


  (define [analyze-begin exp]

    (define [sequentially proc1 proc2]
      (λ [env] (proc1 env) (proc2 env)))

    (define [loop first-proc rest-procs]
      (cond [(null? rest-procs) first-proc]
            [else (loop (sequentially first-proc (car rest-procs))
                        (cdr rest-procs))]))

    (let ([procs (map analyze (cdr exp))])

      (cond [(null? procs) (error "Empty begin")]
            [else (loop (car procs) (cdr procs))])))


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
          [(eq? m 'analyzer) analyze-begin]
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

  (define [analyze-cond exp]
    (let ([cond-proc (analyze (cond->if exp))])
      (λ [env] (cond-proc env))))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'cond]
          [(eq? m 'evaluator) eval-cond]
          [(eq? m 'analyzer) analyze-cond]
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

  (define [list-of-procs exps]
    (if [no-operands? exps]
        '()
        (let ([first-op (analyze (first-operand exps))])
            (cons first-op
                (list-of-procs (rest-operands exps))))))

  (define [execute-application proc args]

    (cond [(premitive-procedure? proc)
           (apply-premitive-procedure proc args)]

          [(compound-procedure? proc)
           ((procedure-body proc)
            (extend-environment (procedure-parameters proc)
                                args
                                (procedure-environment proc)))]

          [else (error "Unknown procedure type: EXECUTE-APPLICATION " proc)]))

  (define [analyze-application exp]
    (let ([operator-proc (analyze (operator exp))]
          [operand-procs (list-of-procs (operands exp))])

      (λ [env] (execute-application (operator-proc env)
                                    (map (λ [operand-proc] (operand-proc env)) operand-procs)))))

  (define [eval-application exp env]
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))

  (define [dispatch m]
    (cond [(eq? m 'tag) null]
          [(eq? m 'evaluator) eval-application]
          [(eq? m 'analyzer) analyze-application]
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
      (cond [(null? exps) TRUE]
            [else
             (eval (make-if (car exps) (cons 'and (cdr exps)) FALSE) env)])))

  (define [analyze-and exp]
    (let ([exps (and-exps exp)])
      (cond [(null? exps) (λ [_env] TRUE)]
            [else
             (λ [env] ((analyze (make-if (car exps)
                                         (cons 'and (cdr exps))
                                         FALSE
                                        )) env))])))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'and]
          [(eq? m 'evaluator) eval-and]
          [(eq? m 'analyzer) analyze-and]
          [else (invalid-msg and-exp m)]))
  dispatch)

(define and-exp-dispatcher [and-exp])
[install-exp 'compound and-exp-dispatcher]


(define [or-exp]
  (define or-exps cdr)

  (define [eval-or exp env]
    (let ([exps (or-exps exp)])
      (cond [(null? exps) FALSE]
            [else
             (eval (make-if (car exps) TRUE (cons 'or (cdr exps))) env)])))

  (define [analyze-or exp]
    (let ([exps (or-exps exp)])

      (cond [(null? exps) (λ [_env] FALSE)]
            [else
             (λ [env] ((analyze (make-if (car exps)
                                         TRUE
                                         (cons 'or (cdr exps)))) env))])))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'or]
          [(eq? m 'evaluator) eval-or]
          [(eq? m 'analyzer) analyze-or]
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

  (define [analyze-let exp]
    (let ([let-proc (analyze (let->combination exp))])
      (λ [env] (let-proc env))))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'let]
          [(eq? m 'evaluator) eval-let]
          [(eq? m 'analyzer) analyze-let]
          [else (invalid-msg let-exp m)]))
  dispatch)

(define let-exp-dispatcher [let-exp])
[install-exp 'compound let-exp-dispatcher]

(define [make-let vars exps]
  (cons 'let (cons vars exps)))

;; TODO Add derived expression to the interpretor for loops like "unless" and "for"

(define [unless-exp]

  (define unless-condition cadr)
  (define unless-value caddr)
  (define unless-exception cadddr)

  (define [analyze-unless exp]
    (let ([condition-proc (analyze (unless-condition exp))]
          [value-proc (analyze (unless-value exp))]
          [exception-proc (analyze (unless-exception exp))])

      (λ [env]
        (if [condition-proc env]
            (exception-proc env) (value-proc env)))))

  (define [eval-unless _exp _env]
    (error "Not Implemented: eval for unless expression")
    )

  (define [dispatch m]
    (cond [(eq? m 'tag) 'unless]
          [(eq? m 'analyzer) analyze-unless]
          [(eq? m 'evaluator) eval-unless]
          [else (invalid-msg unless-exp m)]))

  dispatch)


(define unless-exp-dispatcher [unless-exp])
[install-exp 'compound unless-exp-dispatcher]

#|
  SYNTAX: (letrec ([add-one (λ [x]  (+ x one))]
                   [one 1])
            (add-one 10))

  Transfomation of letrec to let:

            (let ([add-one undefined-value]
                  [one undefined-value])

                (set! add-one (λ [x]  (+ x one)))
                (set! one 1)
                (add-one 10))

  letrec will be transformed to let expression where all the variables
  defined in the letrec expression will have simultaneous scope i.e. they can refer to
  each other while defining.
|#

(define [letrec-exp]

  (define let-variables-list cadr)
  (define variable-name car)
  (define variable-exp cadr)

  (define let-expressions-list cddr)

  (define [letrec->let exp]

    (let* ([variable-names (map variable-name (let-variables-list exp))]

           [variable-expressions (map variable-exp (let-variables-list exp))]

           [undefined-variables (map (λ [var-name] (list var-name undefined-value))
                                     variable-names)]
           [to->set! (λ [d] (cons 'set! d))]

           [initialize-variables-value
            (map to->set!
                 (zip variable-names (map list variable-expressions)))])

      (make-let undefined-variables
                (append initialize-variables-value (let-expressions-list exp)))))

  (define [eval-let-rec exp env]
    (eval (letrec->let exp) env))

  (define [analyze-letrec exp]
    (let ([letrec-proc (analyze (letrec->let exp))])
      (λ [env] (letrec-proc env))))

  (define [dispatch m]
    (cond [(eq? m 'tag) 'letrec]
          [(eq? m 'evaluator) eval-let-rec]
          [(eq? m 'analyzer) analyze-letrec]
          [else (invalid-msg let-rec-exp m)]))

  dispatch)

(define letrec-exp-dispatcher [letrec-exp])
[install-exp 'compound letrec-exp-dispatcher]

(define primitives
  (list
   (cons 'print println)
   (cons 'list list)
   (cons '== eq?)
   (cons 'null null)
   (cons 'cons cons)
   (cons 'car car)
   (cons 'cdr cdr)
   (cons '* *)
   (cons '+ +)
   (cons '- -)
   (cons '= =)
   ))

(provide primitive-frame)
(define primitive-frame
  (make-frame (map car primitives)
              (map cdr primitives)))

(provide base-env)
(define base-env (cons primitive-frame the-empty-environment))
