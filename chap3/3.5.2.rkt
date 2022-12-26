#lang racket

(define (force exp) (exp))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define the-empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (stream-car xs) (car xs))
(define (stream-cdr xs) (force (cdr xs)))

(define stream-null? null?)

;; Stream Manipulators

(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (any pred? xs)
  (cond [(null? xs) #f]
        [(pred? (car xs)) #t]
        [else (any pred? (cdr xs))]))

(define [stream-map proc . streams]
  (if (any stream-null? streams)
      empty-stream
      (cons-stream (apply proc (map stream-car streams))
                   (apply stream-map (cons proc (map stream-cdr streams))))))

(define (stream-filter pred? stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred? (stream-car stream)) (cons-stream
                                       (stream-car stream)
                                       (stream-filter pred? (stream-cdr stream))))
        (else (stream-filter pred? (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 0))

(define (divisible? x y)
  (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define only-sevens
   (stream-filter (lambda (x) (divisible? x 7))
                   integers))

;; (define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
;; (define fibs (fibgen 0 1))

(define (seive-stream stream)
  (let ((prime (stream-car stream))
        )
    (cons-stream prime
                 (seive-stream
                    (stream-filter
                       (lambda (x) (not (divisible? x prime))) (stream-cdr stream))))))

(define primes (seive-stream (integers-starting-from 2)))


(define ones (cons-stream 1 ones))
;; (cons '1 (memo-proc (lambda () ones)))>


(define counter 0) 

(define [1+ x] (+ x 1))

(define (add-streams stream1 stream2) 
    (stream-map 
        (lambda (a b) 
            (begin 
                (set! counter (1+ counter)) 
                (+ a b))) 
        stream1 
        stream2)) 
  

(define fibs (cons-stream 0 (cons-stream 1 (add-streams fibs (stream-cdr fibs)))))
