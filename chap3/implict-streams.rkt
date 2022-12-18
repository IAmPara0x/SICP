#lang racket

(define (force exp) (exp))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define (memo-proc exp)
  (lambda ()
    (let ([already-run? #f] [result #f])
      (if already-run?
          result
          (begin
            (set! already-run? #t)
            (set! result (exp))))
      result)))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ x xs) (cons x (delay xs)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define stream-null? null?)

(define empty-stream '())

;; Utility functions

(define (stream-take! n stream)
  (if (= n 0)
      empty-stream
      (cons (stream-car stream)
            (stream-take! (- n 1) (stream-cdr stream)))))

(define (any pred? xs)
  (cond [(null? xs) #f]
        [(pred? (car xs)) #t]
        [else (any pred? (cdr xs))]))

(define [stream-map proc . streams]
  (if (any stream-null? streams)
      empty-stream
      (cons-stream (apply proc (map stream-car streams))
                   (apply stream-map (cons proc (map stream-cdr streams))))))

;; -------------

(define ones (cons-stream 1 ones))

(define [add-stream s1 s2] (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-stream ones integers)))

(define [scale-stream factor stream]
  (stream-map (λ [x] (* x factor)) stream))

(define [signum n]
  (if (< n 0) -1 1))

(define [assert exp msg]
  (if exp #t (error msg)))

(define [remainder? div num]

  (assert (not [= num 0]) "division by zero")

  (let ([abs-div (abs div)]
        [abs-num (abs num)])

    (define [r n] (* (signum div) (signum num) n))

    (cond [(< abs-div abs-num) (* abs-div (signum div) (signum num))]
          [(= div num) 0]
          [else (r (remainder? (- abs-div abs-num) abs-num))])))


(define [xbonacci . sig]

(define [to-stream xs]
    (if (null? xs)
        empty-stream
        (cons-stream (car xs) (to-stream (cdr xs)))))

(define [snoc-stream ys xs]
  (if (stream-null? xs)
      (force ys)
      (cons-stream (stream-car xs) (snoc-stream ys (stream-cdr xs)))))

  (define [dropwhile n xs]
    (if (= n 0)
        (cons (stream-cdr xs) '())
        (cons (stream-cdr xs) (dropwhile (- n 1) (stream-cdr xs)))))

  (define n (- (length sig) 2))

  (define gen
    (snoc-stream (delay (apply stream-map (cons + (cons gen (dropwhile n gen)))))
                 (to-stream sig)))

  gen)

(define fibs (xbonacci 0 1))
(define tribonnaci (xbonacci 1 1 1))
