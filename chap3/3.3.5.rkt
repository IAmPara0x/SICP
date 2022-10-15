#lang sicp


(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (has-value? connector)
  (connector 'has-value?))

(define (set-value! connector val setter)
  ((connector 'set-value!) val setter))

(define (get-value connector)
  (connector 'value))

(define (forget-value! connector retractor)
  ((connector 'forget!) retractor))

(define (connect connector constraint)
  ((connector 'connect) constraint))

(define (make-connector)

  (let ((value #f)
        (informant #f)
        (constraints '()))

    (define (set-my-value! new-val setter)

      (cond ((not (has-value? me))
             (set! value new-val)
             (set! informant setter)

             (for-each-except setter
                              inform-about-value
                              constraints))

            ((not (= new-val value))
             (error "Contradiction" (list value new-val)))

            (else 'Ignored)))

    (define (forget-my-value! retractor)
      (if (eq? retractor informant)
          (begin (set! informant #f)
                 (set! value #f)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))

      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)

    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value!)
            ((eq? request 'forget!) forget-my-value!)
            ((eq? request 'connect) connect)
            ((eq? request 'constraints) constraints)
            (else (error "Unknown operation: CONNECTOR" request))))
    me))


(define (for-each-except exception procedure list)

  (define (loop items)
    (cond ((null? items) (newline))

          ((eq? exception (car items))
           (loop (cdr items)))

          (else
           (begin
             (procedure (car items))
             (loop (cdr items))))))
  (loop list))

(define (adder a1 a2 sum)

  (define (process-new-value)
    (cond ((and (has-value? a1)
                (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1)
                (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2)
                (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier a1 a2 product)
  (define (process-new-value)

    (cond ((and (has-value? a1)
                (has-value? a2))
           (set-value! product
                       (* (get-value a1) (get-value a2)) me))

          ((and (has-value? a1)
                (has-value? product))

           (if (= (get-value a1) 0)
               (error "Division by Zero.")
               (set-value! a2
                           (/ (get-value product) (get-value a1))
                           me)))

          ((and (has-value? a2)
                (has-value? product))

           (if (= (get-value a2) 0)
               (error "Division by Zero.")
               (set-value! a1
                           (/ (get-value product) (get-value a2))
                           me)))))

  (define (process-forget-value)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (forget-value! product me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect product me)
  me)

(define (constant value connector)

  (define (me request)
    (error "Unknown request: CONSTANT" request))

  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)

  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))

  (define (process-new-value)
    (print-probe (get-value connector)))

  (define (process-forget-value)
    (print-probe "?"))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: PROBE" request))))

  (connect connector me)
  (newline)
  me)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))

    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
   'ok))


(define C (make-connector))
(define F (make-connector))


(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit" F)

(set-value! C 25 'user)
(forget-value! C 'user)
(set-value! F 212 'user)

(define (average a b c )

  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (constant 2 v)
    (multiplier c v u)
    'ok))

(define A (make-connector))
(define B (make-connector))
(define O (make-connector))

(average A B O)

(probe "Input-A" A)
(probe "Input-B" B)
(probe "Output-C" O)

(set-value! A 10 'user)
(set-value! O 4 'user)

