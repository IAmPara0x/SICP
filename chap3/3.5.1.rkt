#lang sicp

;; Utility procedure
(define (assert p s)
  (if (not p) (error s) #t))

(define (force exp) (exp))

;; Lazy evaluation
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

;; (define-syntax delay
;;   (syntax-rules ()
;;     ((_ exp) (lambda () exp))))

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

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-map proc xs)
  (cond ((stream-null? xs) the-empty-stream)
        (else (cons-stream (proc (stream-car xs))
                           (stream-map proc (stream-cdr xs))))))

(define (stream-filter p? xs)
  (cond ((stream-null? xs) the-empty-stream)
        ((p? (stream-car xs)) (cons-stream
                                (stream-car xs)
                                (stream-filter p? (stream-cdr xs))))
        (else (stream-filter p? (stream-cdr xs)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval a b)
   (if (> a b)
       the-empty-stream
       (cons-stream a (stream-enumerate-interval (+ a 1) b))))

(define (stream-map-gen proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream

      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map cdr argstreams))))))

(define (show x) (display x) (newline) x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

(newline)

(define sum 0)

(define (accum x) (set! sum (+ x sum)) sum)

(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z (stream-filter
           (lambda (x) (= (remainder x 5) 0))
           seq))

(display "Ex: 3.52")
(newline)
(stream-ref y 7)
(display-stream z)
